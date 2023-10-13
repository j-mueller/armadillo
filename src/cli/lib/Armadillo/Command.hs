{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
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
  localCreatePool,
  createToken,
  makeDeposit,
  localMakeDeposit,
  applyDeposit,
  makeRedemption,
  applyRedemption
) where

import           Armadillo.BuildTx                  (DEXBuildTxError,
                                                     DepositOutput (..),
                                                     PoolOutput (..),
                                                     PoolState (..),
                                                     RedeemOutput (..),
                                                     ReferenceScripts,
                                                     initialLiquidity)
import qualified Armadillo.BuildTx                  as BuildTx
import           Armadillo.Kupo                     (KupoUtxoQueryT,
                                                     runKupoUtxoQueryT)
import           Armadillo.Scripts                  (Scripts)
import           Cardano.Api                        (AssetId, AssetName,
                                                     CardanoMode, Env,
                                                     LocalNodeConnectInfo,
                                                     PaymentCredential,
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
                                                     balanceAndSubmitOperator,
                                                     balanceOperator,
                                                     signAndSubmitOperator,
                                                     utxosByPayment)
import           Convex.Utils                       (mapError, txnUtxos)
import           Convex.Wallet.Operator             (Operator (oPaymentKey),
                                                     Signing,
                                                     operatorPaymentCredential,
                                                     verificationKey)
import           Data.Functor                       (($>))
import qualified Data.Map                           as Map
import           Data.Maybe                         (listToMaybe)
import           ErgoDex.CardanoApi                 (CardanoApiScriptError)
import           ErgoDex.Contracts.Pool             (PoolConfig, maxLqCap)
import           GHC.Generics                       (Generic)
import           Servant.Client                     (ClientEnv)


runBlockchainAction :: Functor m => LocalNodeConnectInfo CardanoMode -> Env -> ClientEnv -> MonadBlockchainWaitingT (MonadBlockchainCardanoNodeT TxCommandError (KupoUtxoQueryT (ExceptT TxCommandError m))) a -> m (Either (MonadBlockchainError TxCommandError) a)
runBlockchainAction connectInfo nodeEnv env action =
  let mapErr (Left err)         = Left (MonadBlockchainError err) -- FIXME: This needs to be fixed in MonadBlockchainCardanoNodeT!
      mapErr (Right (Left err)) = Left err
      mapErr (Right (Right x))  = Right x
  in fmap mapErr
      $ runExceptT
      $ runKupoUtxoQueryT env
        $ runMonadBlockchainCardanoNodeT connectInfo
          $ runMonadBlockchainWaitingT connectInfo nodeEnv action

data TxCommandError =
  ScriptErr CardanoApiScriptError
  | BalanceSubmitFailed BalanceAndSubmitError
  | NoSuitableInputFound PaymentCredential
  | BuildTxError DEXBuildTxError
  | NoSuitablePoolFound C.AssetId C.AssetId
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

initialState :: Quantity -> Quantity -> PoolState
initialState (Quantity resX) (Quantity resY) =
  PoolState
    { reservesX = Quantity resX
    , reservesY = Quantity resY
    , liquidity = initialLiquidity (Quantity $ ceiling @Double $ sqrt $ fromIntegral $ resX * resY)
    }

{-| Build a (balanced, unsigned) transaction that creates a new pool
-}
createPool :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Scripts -> PaymentCredential -> Integer -> (AssetId, Quantity) -> (AssetId, Quantity) -> m (C.Tx C.BabbageEra, PoolOutput TxIn)
createPool scripts paymentCredential fee pairX pairY = do
  let numTokens = Quantity maxLqCap
  (txi, _) <- selectCredentialUtxo paymentCredential >>= maybe (throwError $ NoSuitableInputFound paymentCredential) pure
  (out, btx) <- runBuildTxT $ mapError ScriptErr $ do
    nft <- BuildTx.createPoolNft txi
    lqToken <- BuildTx.createPoolLiquidityToken txi numTokens
    let poolCfg = BuildTx.poolConfig
                    (PL.transAssetId $ fst pairX) (PL.transAssetId $ fst pairY)
                    fee
                    lqToken
                    nft
    BuildTx.addPoolOutput scripts poolCfg (initialState (snd pairX) (snd pairY))
  tx <- mapError BalanceSubmitFailed (balanceOperator paymentCredential Nothing (btx emptyTx))
  pure (tx, out $> fst (head (txnUtxos tx)))

{-| Create a new LP pool using the given operator's funds and private key
-}
localCreatePool :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Scripts -> CreatePoolParams -> m (PoolOutput TxIn)
localCreatePool scripts CreatePoolParams{cppOperator, cppFee, cppAssetClassX, cppAssetClassY} = do
  (plTx, plOut) <- createPool scripts (operatorPaymentCredential cppOperator) cppFee cppAssetClassX cppAssetClassY
  _finalTx <- mapError BalanceSubmitFailed (signAndSubmitOperator cppOperator plTx)
  pure plOut

createToken :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Operator Signing -> AssetName -> Quantity -> m (C.Tx C.BabbageEra, ScriptHash)
createToken operator name quantity = do
  (scriptHash, btx) <- runBuildTxT $ BuildTx.mintToken name quantity
  x <- mapError BalanceSubmitFailed (balanceAndSubmitOperator operator Nothing (btx emptyTx))
  pure (x, scriptHash)

{-| Build a (balanced, unsigned) transaction that makes a deposit
-}
makeDeposit :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Scripts -> C.Hash C.PaymentKey -> Maybe (C.Hash C.StakeKey) -> PoolConfig -> (Quantity, Quantity) -> m (C.Tx C.BabbageEra, DepositOutput TxIn)
makeDeposit scripts paymentKey stakeKey poolCfg quantities = do
  (out, btx) <- runBuildTxT $ mapError BuildTxError $ BuildTx.deposit scripts paymentKey stakeKey poolCfg quantities
  tx <- mapError BalanceSubmitFailed (balanceOperator (C.PaymentCredentialByKey paymentKey) Nothing (btx emptyTx))
  pure (tx, out $> fst (head (txnUtxos tx)))

localMakeDeposit :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Scripts -> Operator Signing -> PoolConfig -> Quantity -> m (DepositOutput TxIn)
localMakeDeposit scripts operator poolCfg quantity = do
  let pkh = C.verificationKeyHash $ verificationKey $ oPaymentKey operator
  (tx, txOut) <- makeDeposit scripts pkh Nothing poolCfg (quantity, quantity)
  _finalTx <- mapError BalanceSubmitFailed (signAndSubmitOperator operator tx)
  pure txOut

applyDeposit :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => ReferenceScripts TxIn -> Scripts -> Operator Signing -> DepositOutput TxIn -> PoolOutput TxIn -> m (PoolOutput TxIn)
applyDeposit refScripts scripts operator deposit pool = do
  ((poolOut, rewardOut), btx) <- runBuildTxT $ mapError BuildTxError $ BuildTx.applyDeposit refScripts scripts operator deposit pool
  tx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator operator (Just rewardOut) (btx emptyTx))
  pure $ poolOut $> fst (head (txnUtxos tx))

makeRedemption :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Scripts -> Operator Signing -> PoolConfig -> Quantity -> m (RedeemOutput TxIn)
makeRedemption scripts operator poolCfg quantity = do
  let pkh = C.verificationKeyHash $ verificationKey $ oPaymentKey operator
  (out, btx) <- runBuildTxT $ mapError BuildTxError $ BuildTx.redeem scripts pkh Nothing poolCfg quantity
  tx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator operator Nothing (btx emptyTx))
  pure $ out $> fst (head (txnUtxos tx))

applyRedemption :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => ReferenceScripts TxIn -> Scripts -> Operator Signing -> RedeemOutput TxIn -> PoolOutput TxIn -> m (PoolOutput TxIn)
applyRedemption refScripts scripts operator deposit pool = do
  ((poolOut, rewardOut), btx) <- runBuildTxT $ mapError BuildTxError $ BuildTx.applyRedemption refScripts scripts operator deposit pool
  tx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator operator (Just rewardOut) (btx emptyTx))
  pure $ poolOut $> fst (head (txnUtxos tx))

{-| Select a single UTxO that is locked by the payment credential. |-}
selectCredentialUtxo :: MonadUtxoQuery m => PaymentCredential -> m (Maybe (C.TxIn, C.TxOut C.CtxUTxO C.BabbageEra))
selectCredentialUtxo = fmap (listToMaybe . Map.toList . C.unUTxO) . utxosByPayment
