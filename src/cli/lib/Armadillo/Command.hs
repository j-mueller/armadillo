{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections  #-}
{-| Running blockchain actions
-}
module Armadillo.Command(
  TxCommandError(..),
  runBlockchainAction,

  -- * Individual commands
  deployRefScripts,
  ActivePool(..),
  CreatePoolParams(..),
  initialState,
  createPool,
  createToken
) where

import           Armadillo.BuildTx                  (PoolOutput (..),
                                                     ReferenceScripts)
import qualified Armadillo.BuildTx                  as BuildTx
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
import           Convex.Wallet.Operator             (Operator, Signing)
import           Data.Aeson                         (FromJSON, ToJSON)
import           ErgoDex.CardanoApi                 (CardanoApiScriptError)
import           ErgoDex.Contracts.Pool             (PoolState (..), maxLqCap)
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
  deriving (Show)

deployRefScripts ::
  ( MonadBlockchain m
  , MonadError TxCommandError m
  , MonadUtxoQuery m
  ) =>
  Operator Signing ->
  m (ReferenceScripts TxIn)
deployRefScripts operator = do
  (cs, btx) <- mapError ScriptErr $ runBuildTxT $ do
    r <- BuildTx.deployReferenceScripts operator
    queryProtocolParameters >>= BuildTx.setMinAdaDepositAll
    pure r
  finalTx <- mapError BalanceSubmitFailed $ balanceAndSubmitOperator operator Nothing (btx emptyTx)
  let txId = C.getTxId $ C.getTxBody finalTx
      mkIdx i = C.TxIn txId (C.TxIx $ fromIntegral i)
  pure (mkIdx <$> cs)

-- | An active liquidity pool
data ActivePool =
  ActivePool
    { apPoolOutput :: PoolOutput
    , apTxOut      :: (C.TxIn, C.TxOut C.CtxTx C.BabbageEra)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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
createPool :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => CreatePoolParams -> m ActivePool
createPool params@CreatePoolParams{cppOperator, cppFee, cppAssetClassX, cppAssetClassY} = do
  let numTokens = Quantity maxLqCap
  (txi, _) <- selectOperatorUTxO cppOperator >>= maybe (throwError $ NoSuitableInputFound cppOperator) pure
  ((txOut, poConfig, poNFT, poLiquidity, poState), btx) <- runBuildTxT $ mapError ScriptErr $ do
    nft <- BuildTx.createPoolNft txi
    lqToken <- BuildTx.createPoolLiquidityToken txi numTokens
    let cfg = BuildTx.poolConfig
                (PL.transAssetId $ fst cppAssetClassX) (PL.transAssetId $ fst cppAssetClassY)
                cppFee
                lqToken
                nft
    let iState = initialState params
    (, cfg, nft, lqToken, iState) <$> BuildTx.addPoolOutput lqToken nft cfg iState
  let apPoolOutput =
        PoolOutput
          { poTxOut = txOut
          , poConfig
          , poState
          , poNFT
          , poLiquidity
          }

  tx <- mapError BalanceSubmitFailed (balanceAndSubmitOperator cppOperator Nothing (btx emptyTx))
  pure ActivePool{apPoolOutput, apTxOut = head (txnUtxos tx)}

createToken :: (MonadUtxoQuery m, MonadBlockchain m, MonadError TxCommandError m) => Operator Signing -> AssetName -> Quantity -> m (C.Tx C.BabbageEra, ScriptHash)
createToken operator name quantity = do
  (scriptHash, btx) <- runBuildTxT $ BuildTx.mintToken name quantity
  x <- mapError BalanceSubmitFailed (balanceAndSubmitOperator operator Nothing (btx emptyTx))
  pure (x, scriptHash)

