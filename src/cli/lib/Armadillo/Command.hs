{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections  #-}
{-| Running blockchain actions
-}
module Armadillo.Command(
  TxCommandError(..),
  runBlockchainAction,

  -- * Individual commands
  deployRefScripts,
  CreatePoolParams(..),
  initialState,
  createPool,
  createToken,
  makeDeposit
) where

import           Armadillo.BuildTx                  (DEXBuildTxError,
                                                     DepositOutput (..),
                                                     PoolOutput (..),
                                                     ReferenceScripts)
import qualified Armadillo.BuildTx                  as BuildTx
import           Armadillo.Scripts                  (Scripts)
import           Cardano.Api                        (AssetId, AssetName,
                                                     CardanoMode, Env,
                                                     LocalNodeConnectInfo,
                                                     Quantity (..), ScriptHash,
                                                     TxIn)
import qualified Cardano.Api                        as C
import           Control.Monad.Except               (ExceptT, MonadError,
                                                     runExceptT, throwError)
import           Convex.BuildTx                     (runBuildTxT)
import qualified Convex.BuildTx                     as BuildTx
import           Convex.Class                       (MonadBlockchain,
                                                     MonadBlockchainCardanoNodeT,
                                                     MonadBlockchainError (..),
                                                     queryProtocolParameters,
                                                     runMonadBlockchainCardanoNodeT)
import           Convex.Lenses                      (emptyTx)
import           Convex.NodeClient.WaitForTxnClient (MonadBlockchainWaitingT,
                                                     runMonadBlockchainWaitingT)
import qualified Convex.PlutusLedger                as PL
import           Convex.Query                       (BalanceAndSubmitError,
                                                     MonadUtxoQuery,
                                                     WalletAPIQueryT,
                                                     balanceAndSubmitOperator,
                                                     runWalletAPIQueryT,
                                                     selectOperatorUTxO)
import           Convex.Utils                       (mapError, txnUtxos)
import           Convex.Wallet.Operator             (Operator (oPaymentKey),
                                                     Signing, verificationKey)
import           Data.Functor                       (($>))
import           ErgoDex.CardanoApi                 (CardanoApiScriptError)
import           ErgoDex.Contracts.Pool             (PoolConfig, PoolState (..),
                                                     maxLqCap)
import           GHC.Generics                       (Generic)
import           Servant.Client                     (ClientEnv)


runBlockchainAction :: Functor m => LocalNodeConnectInfo CardanoMode -> Env -> ClientEnv -> MonadBlockchainWaitingT (MonadBlockchainCardanoNodeT TxCommandError (WalletAPIQueryT (ExceptT TxCommandError m))) a -> m (Either (MonadBlockchainError TxCommandError) a)
runBlockchainAction connectInfo nodeEnv env action =
  let mapErr (Left err)         = Left (MonadBlockchainError err) -- FIXME: This needs to be fixed in MonadBlockchainCardanoNodeT!
      mapErr (Right (Left err)) = Left err
      mapErr (Right (Right x))  = Right x
  in fmap mapErr
      $ runExceptT
      $ runWalletAPIQueryT env
        $ runMonadBlockchainCardanoNodeT connectInfo
          $ runMonadBlockchainWaitingT connectInfo nodeEnv action

data TxCommandError =
  ScriptErr CardanoApiScriptError
  | BalanceSubmitFailed BalanceAndSubmitError
  | NoSuitableInputFound (Operator Signing)
  | BuildTxError DEXBuildTxError
  deriving (Show)

deployRefScripts ::
  ( MonadBlockchain m
  , MonadError TxCommandError m
  , MonadUtxoQuery m
  ) =>
  Scripts ->
  Operator Signing ->
  m (ReferenceScripts TxIn)
deployRefScripts scripts operator = do
  (cs, btx) <- mapError ScriptErr $ runBuildTxT $ do
    r <- BuildTx.deployReferenceScripts scripts operator
    queryProtocolParameters >>= BuildTx.setMinAdaDepositAll
    pure r
  finalTx <- mapError BalanceSubmitFailed $ balanceAndSubmitOperator operator Nothing (btx emptyTx)
  let txId = C.getTxId $ C.getTxBody finalTx
      mkIdx i = C.TxIn txId (C.TxIx $ fromIntegral i)
  pure (mkIdx <$> cs)

data CreatePoolParams =
  CreatePoolParams
    { cppOperator    :: Operator Signing
    , cppFee         :: Integer -- ^ Fee in 1/1000th of the traded amount
    , cppAssetClassX :: (AssetId, Quantity)
    , cppAssetClassY :: (AssetId, Quantity)
    }
    deriving stock (Eq, Show, Generic)

initialState :: CreatePoolParams -> PoolState
initialState CreatePoolParams{cppAssetClassX=(_, Quantity reservesX), cppAssetClassY=(_, Quantity reservesY)} =
  PoolState
    { reservesX
    , reservesY
    , liquidity = maxLqCap
    }

{-| Create a new LP pool
-}
createPool :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Scripts -> CreatePoolParams -> m (PoolOutput TxIn)
createPool scripts params@CreatePoolParams{cppOperator, cppFee, cppAssetClassX, cppAssetClassY} = do
  let numTokens = Quantity maxLqCap
  (txi, _) <- selectOperatorUTxO cppOperator >>= maybe (throwError $ NoSuitableInputFound cppOperator) pure
  (out, btx) <- runBuildTxT $ mapError ScriptErr $ do
    nft <- BuildTx.createPoolNft txi
    lqToken <- BuildTx.createPoolLiquidityToken txi numTokens
    let poolCfg = BuildTx.poolConfig
                    (PL.transAssetId $ fst cppAssetClassX) (PL.transAssetId $ fst cppAssetClassY)
                    cppFee
                    lqToken
                    nft
    BuildTx.addPoolOutput scripts poolCfg (initialState params)
  tx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator cppOperator Nothing (btx emptyTx))
  pure $ out $> fst (head (txnUtxos tx))

createToken :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Operator Signing -> AssetName -> Quantity -> m (C.Tx C.BabbageEra, ScriptHash)
createToken operator name quantity = do
  (scriptHash, btx) <- runBuildTxT $ BuildTx.mintToken name quantity
  x <- mapError BalanceSubmitFailed (balanceAndSubmitOperator operator Nothing (btx emptyTx))
  pure (x, scriptHash)

makeDeposit :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Scripts -> Operator Signing -> PoolConfig -> Quantity -> m (DepositOutput TxIn)
makeDeposit scripts operator poolCfg quantity = do
  let pkh = C.verificationKeyHash $ verificationKey $ oPaymentKey operator
  (out, btx) <- runBuildTxT $ mapError BuildTxError $ BuildTx.deposit scripts pkh Nothing poolCfg quantity
  tx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator operator Nothing (btx emptyTx))
  pure $ out $> fst (head (txnUtxos tx))
