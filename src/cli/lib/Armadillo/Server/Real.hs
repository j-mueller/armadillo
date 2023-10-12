{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-| The "real" implementation of the HTTP server, using data from the chain
follower and the DB
-}
module Armadillo.Server.Real(
  TxBuildingContext(..),
  mkTxBuildingContext,
  runTxBuildAction,

  historicAPI,
  internalAPI,
  buildTxAPI,
  fromCardanoAssetId
) where

import           Armadillo.Api                        (AssetID (..), BuildTxAPI,
                                                       CreatePoolArgs (..),
                                                       HistoricAPI, InternalAPI,
                                                       Pair (..),
                                                       WrappedTx (..), mkPair,
                                                       toCardanoAssetId)
import           Armadillo.BuildTx                    (DepositOutput (..),
                                                       PoolOutput (..))
import           Armadillo.ChainFollower.DepositState (DepositState (..))
import           Armadillo.ChainFollower.PoolState    (PoolUtxoState (..))
import           Armadillo.ChainFollower.State        (ChainFollowerState (..))
import           Armadillo.Cli.Command                (NodeClientConfig,
                                                       localNodeConnectInfo)
import qualified Armadillo.Command                    as Command
import           Armadillo.Kupo                       (KupoConfig,
                                                       KupoUtxoQueryT,
                                                       kupoClientEnv,
                                                       runKupoUtxoQueryT)
import           Armadillo.Scripts                    (Scripts,
                                                       loadScriptsConfig,
                                                       scriptsFromScriptsConfig)
import qualified Armadillo.Server.Mock                as M
import           Cardano.Api                          (Quantity (..))
import qualified Cardano.Api                          as C
import           Control.Concurrent.STM               (TVar, atomically,
                                                       readTVar)
import           Control.Monad.Except                 (MonadError (..),
                                                       runExcept, throwError)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Convex.Class                         (MonadBlockchainCardanoNodeT,
                                                       MonadBlockchainError,
                                                       runMonadBlockchainCardanoNodeT)
import           Convex.MonadLog                      (runMonadLogIgnoreT)
import           Convex.PlutusLedger                  (unTransAssetId,
                                                       unTransPubKeyHash)
import           Convex.Utils                         (liftEither, mapError)
import qualified Convex.Utxos
import           Data.Foldable                        (toList)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes)
import           Data.String                          (IsString (..))
import           ErgoDex.Contracts.Pool               (PoolConfig (..))
import           Network.HTTP.Client                  (defaultManagerSettings,
                                                       newManager)
import           PlutusLedgerApi.V1.Value             (AssetClass)
import           Servant.API                          ((:<|>) (..))
import           Servant.Client                       (ClientEnv)
import           Servant.Server                       (Server, ServerError,
                                                       err500, errBody)

historicAPI :: TVar ChainFollowerState -> Server HistoricAPI
historicAPI tv =
  getPairs tv
  :<|> M.getHistoric
  :<|> M.getBuyTxns
  :<|> M.getSellTxns
  :<|> M.getAllTxns
  :<|> M.getChartForPair
  :<|> M.getChartForDex

buildTxAPI :: TxBuildingContext -> TVar ChainFollowerState -> Server BuildTxAPI
buildTxAPI ctx tv =
  createPoolTx ctx tv

data ConversionError =
  PlutusToCardanoApiError C.SerialiseAsRawBytesError
  deriving Show

getPairs :: MonadIO m => TVar ChainFollowerState -> m [Pair]
getPairs tv = do
  ChainFollowerState{cfsPoolState=PoolUtxoState{_utxos=Convex.Utxos.UtxoSet{Convex.Utxos._utxos}}} <- liftIO (atomically (readTVar tv))
  catMaybes <$> traverse (getPair . poConfig . snd . snd) (Map.toList _utxos)

getPair :: MonadIO m => PoolConfig -> m (Maybe Pair)
getPair cfg = case runExcept (configPair cfg) of
  Right x -> pure (Just x)
  Left err -> do
    liftIO (putStrLn ("getPair: Failed: " <> show err)) -- FIXME Logging
    pure Nothing

configPair :: MonadError ConversionError m => PoolConfig -> m Pair
configPair PoolConfig{poolX, poolY} =
  mkPair <$> assetClassToAssetID poolX <*> assetClassToAssetID poolY

assetClassToAssetID :: MonadError ConversionError m => AssetClass -> m AssetID
assetClassToAssetID =
  fmap fromCardanoAssetId . either (throwError . PlutusToCardanoApiError) pure . unTransAssetId

fromCardanoAssetId :: C.AssetId -> AssetID
fromCardanoAssetId = AssetID . (\case
  C.AdaAssetId -> "ada"
  C.AssetId policyId assetName ->
    C.serialiseToRawBytesHexText policyId <> "." <> C.serialiseToRawBytesHexText assetName
    )

internalAPI :: TVar ChainFollowerState -> Server InternalAPI
internalAPI tv =
  getPools tv :<|> getDeposits tv

getPools :: MonadIO m => TVar ChainFollowerState -> m [PoolOutput C.TxIn]
getPools tv = do
  ChainFollowerState{cfsPoolState=PoolUtxoState{_utxos=(Convex.Utxos.UtxoSet u)}} <- liftIO (atomically (readTVar tv))
  pure $ fmap snd $ toList u

getDeposits :: MonadIO m => TVar ChainFollowerState -> m [DepositOutput C.TxIn]
getDeposits tv = do
  ChainFollowerState{cfsDepositState=DepositState{_depositUtxos=(Convex.Utxos.UtxoSet u)}} <- liftIO (atomically (readTVar tv))
  pure $ fmap snd $ toList u

mkTxBuildingContext :: KupoConfig -> NodeClientConfig -> IO TxBuildingContext
mkTxBuildingContext kupoCfg cfg = do
  (nodeConnectInfo, nodeEnv) <- localNodeConnectInfo cfg
  scripts <- loadScriptsConfig >>= scriptsFromScriptsConfig >>= either (fail . (<>) "mkBuildingContext: DecoderError: " . show) pure
  kupoClient <- kupoClientEnv <$> newManager defaultManagerSettings <*> pure kupoCfg
  pure TxBuildingContext{nodeConnectInfo, nodeEnv, scripts, kupoClient}

{-| Connection data that's used for building transactions
-}
data TxBuildingContext =
  TxBuildingContext
    { nodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode -- ^ Connecting to the local cardano node
    , nodeEnv         :: C.Env -- ^ cardano node client env
    , scripts         :: Scripts
    , kupoClient      :: ClientEnv
    }

{-| Use the @TxBuildingContext@ to run an action that queries the local node and the chain index
-}
runTxBuildAction :: forall e m a. (Show e, MonadError ServerError m) => TxBuildingContext -> MonadBlockchainCardanoNodeT e (KupoUtxoQueryT m) a -> m a
runTxBuildAction TxBuildingContext{nodeConnectInfo, kupoClient} action = do
  runKupoUtxoQueryT kupoClient $ liftEither fromBlockchainError $ runMonadBlockchainCardanoNodeT nodeConnectInfo action

{-| Convert the @MonadBlockchainError@ to a @ServerError@
-}
fromBlockchainError :: (Show e) => MonadBlockchainError e -> ServerError
fromBlockchainError err = err500{errBody = fromString ("cardano-node action failed: " <> show err)}

fromBuildTxError :: BuildTxError -> ServerError
fromBuildTxError err = err500{errBody = fromString ("build tx action failed: " <> show err)}

createPoolTx :: forall m. (MonadIO m, MonadError ServerError m) => TxBuildingContext -> TVar ChainFollowerState -> CreatePoolArgs -> m WrappedTx
createPoolTx ctx@TxBuildingContext{scripts} tvar args@CreatePoolArgs{cpoFeeNumerator, cpoAssetX, cpoAssetY, cpoQuantityX, cpoQuantityY, cpoPublicKeyHash, cpoStakingCredential} = runMonadLogIgnoreT $ runTxBuildAction ctx $ do
  paymentCred <- C.PaymentCredentialByKey <$> liftEither (fromBuildTxError . SerialisationError) (pure $ unTransPubKeyHash cpoPublicKeyHash)
  (assetX, assetY) <- (,) <$> parseAssetID cpoAssetX <*> parseAssetID cpoAssetY
  (tx, _) <- mapError (fromBuildTxError . TxCommandError) (Command.createPool scripts paymentCred  cpoFeeNumerator (assetX, Quantity cpoQuantityX) (assetY, Quantity cpoQuantityY))
  undefined

data BuildTxError =
  SerialisationError C.SerialiseAsRawBytesError
  | TxCommandError Command.TxCommandError
  | AssetIdParseFailed String
  deriving Show

parseAssetID :: MonadError ServerError m => AssetID -> m C.AssetId
parseAssetID = liftEither (fromBuildTxError . AssetIdParseFailed) . pure . toCardanoAssetId
