{-# LANGUAGE OverloadedStrings #-}
{-| The "real" implementation of the HTTP server, using data from the chain
follower and the DB
-}
module Armadillo.Server.Real(
  historicAPI,
  internalAPI,
  fromCardanoAssetId
) where

import           Armadillo.Api                        (AssetID (..),
                                                       HistoricAPI, InternalAPI,
                                                       Pair (..), mkPair)
import           Armadillo.BuildTx                    (DepositOutput (..),
                                                       PoolOutput (..))
import           Armadillo.ChainFollower.DepositState (DepositState (..))
import           Armadillo.ChainFollower.PoolState    (PoolUtxoState (..))
import           Armadillo.ChainFollower.State        (ChainFollowerState (..))
import qualified Armadillo.Server.Mock                as M
import qualified Cardano.Api                          as C
import           Control.Concurrent.STM               (TVar, atomically,
                                                       readTVar)
import           Control.Monad.Except                 (MonadError (..),
                                                       runExcept, throwError)
import           Control.Monad.IO.Class               (MonadIO (..))
import           Convex.PlutusLedger                  (unTransAssetId)
import qualified Convex.Utxos
import           Data.Foldable                        (toList)
import qualified Data.Map                             as Map
import           Data.Maybe                           (catMaybes)
import           ErgoDex.Contracts.Pool               (PoolConfig (..))
import           PlutusLedgerApi.V1.Value             (AssetClass)
import           Servant.API                          ((:<|>) (..))
import           Servant.Server                       (Server)

historicAPI :: TVar ChainFollowerState -> Server HistoricAPI
historicAPI tv =
  getPairs tv
  :<|> M.getHistoric
  :<|> M.getBuyTxns
  :<|> M.getSellTxns
  :<|> M.getAllTxns
  :<|> M.getChartForPair
  :<|> M.getChartForDex

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
