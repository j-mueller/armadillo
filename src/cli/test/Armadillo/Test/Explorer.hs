{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE ViewPatterns      #-}
{-| Mock implementation of spectrum finance explorer
used for integration tests
-}
module Armadillo.Test.Explorer(
  RunningExplorer(..),
  ExplorerLog(..),
  ExplorerPort(..),
  startExplorer
  ) where

import           Armadillo.Orphans          ()
import           Armadillo.Test.AMMExecutor (ExplorerConfig (..), Network (..),
                                             Uri (..))
import qualified Cardano.Api                as C
import qualified Cardano.Api.Shelley        as C
import           Control.Concurrent         (forkIO)
import           Control.Lens               (view)
import           Control.Monad              (void)
import           Control.Monad.Except       (MonadError (throwError))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Tracer             (Tracer, traceWith)
import           Convex.Devnet.CardanoNode  (RunningNode (..))
import qualified Convex.Lenses              as L
import           Convex.NodeQueries         (queryLocalState)
import           Convex.PlutusLedger        (unTransTxOutRef)
import           Data.Aeson                 (FromJSON (..), ToJSON (..), object,
                                             (.=))
import qualified Data.Map                   as Map
import           Data.Proxy                 (Proxy (..))
import qualified Data.Set                   as Set
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           GHC.Generics               (Generic)
import qualified Network.Wai.Handler.Warp   as Warp
import qualified PlutusLedgerApi.V1.Scripts as P
import           PlutusLedgerApi.V1.Tx      (TxOutRef (..))
import qualified PlutusTx
import qualified PlutusTx.Prelude           as PlutusTx
import           Servant.API                (Capture, Get, JSON, type (:>))
import           Servant.Server             (Server, ServerError, err400,
                                             errBody, serve)

data ExplorerLog =
  TxOutRequest{ rqNetwork :: Network, rqOutRef :: TxOutRef}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RunningExplorer =
  RunningExplorer
    { reUri    :: String
    , rePort   :: ExplorerPort
    , reConfig :: ExplorerConfig
    }

newtype ExplorerPort = ExplorerPort{getExplorerPort :: Int }

localUri :: ExplorerPort -> Uri
localUri (ExplorerPort p) = Uri $ "http://localhost:" <> show p

startExplorer :: Tracer IO ExplorerLog -> RunningNode -> ExplorerPort -> IO RunningExplorer
startExplorer tracer rn rePort = do
  void $ forkIO $ runServer tracer rn rePort
  pure RunningExplorer
        { reUri = "localhost"
        , rePort
        , reConfig = ExplorerConfig{explorerUri = localUri rePort, network=Preview}}

type ExplorerAPI =
  "cardano" :> Capture "network" Network :> "v1" :> "outputs" :> Capture "txOutRef" TxOutRef :> Get '[JSON] (Maybe FullTxOut)

data FullTxOut = FullTxOut
  { fullTxOutRef       :: C.TxIn
  , fullTxOutAddress   :: C.Address C.ShelleyAddr
  , fullTxOutValue     :: [OutAsset]
  , fullTxOutDatum     :: TxOutDatum
  , fullTxOutScriptRef :: Maybe Text
  } deriving (Show, Eq, Generic, FromJSON)

instance ToJSON FullTxOut where
  toJSON FullTxOut{fullTxOutRef, fullTxOutAddress, fullTxOutValue, fullTxOutScriptRef} =
    let C.TxIn txId (C.TxIx ix) = fullTxOutRef
    in object
        [ "ref" .= (C.serialiseToRawBytesHexText txId <> ":" <> Text.pack (show ix))
        , "txHash" .= C.serialiseToRawBytesHexText txId
        , "index" .= ix
        , "addr" .= C.serialiseAddress fullTxOutAddress
        , "value" .= fullTxOutValue
        , "refScriptHash" .= fullTxOutScriptRef
        , "globalIndex" .= (100 :: Int) -- ?
        ]

data TxOutDatum
  = KnownDatum P.Datum
  | KnownDatumHash P.DatumHash
  | EmptyDatum
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

runServer :: Tracer IO ExplorerLog -> RunningNode -> ExplorerPort -> IO ()
runServer tracer rn ExplorerPort{getExplorerPort} =
  let app = serve (Proxy @ExplorerAPI) (server tracer rn)
  in Warp.run getExplorerPort app

server :: Tracer IO ExplorerLog -> RunningNode ->  Server ExplorerAPI
server tr = getOutput tr

getOutput :: (MonadError ServerError m, MonadIO m) => Tracer IO ExplorerLog -> RunningNode -> Network -> TxOutRef -> m (Maybe FullTxOut)
getOutput tracer RunningNode{rnConnectInfo} n outRef = do
  txI <- either (const $ throwError err400) pure (unTransTxOutRef outRef)
  C.UTxO k <- liftIO (queryLocalState (C.QueryInEra C.BabbageEraInCardanoMode (C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage (C.QueryUTxO (C.QueryUTxOByTxIn (Set.singleton txI))))) (fst rnConnectInfo)) >>= either (const $ throwError err400) pure
  liftIO (traceWith tracer $ TxOutRequest n outRef)
  case Map.lookup txI k of
    Nothing -> pure Nothing
    Just (view L._TxOut -> (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr, view L._TxOutValue -> vl, dat, rs))  -> do
      pure $ Just $
        FullTxOut
          { fullTxOutRef = txI
          , fullTxOutAddress = addr
          , fullTxOutValue = valueToOutAsset vl
          , fullTxOutDatum = case dat of
              C.TxOutDatumNone -> EmptyDatum
              C.TxOutDatumHash _ hsh -> KnownDatumHash (P.DatumHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes hsh)
              C.TxOutDatumInline _ (PlutusTx.dataToBuiltinData . C.toPlutusData . C.getScriptData -> dat') -> KnownDatum (P.Datum dat')
          , fullTxOutScriptRef = case rs of
              C.ReferenceScriptNone -> Nothing
              C.ReferenceScript _ (C.ScriptInAnyLang _ script) -> Just (C.serialiseToRawBytesHexText $ C.hashScript $ script)
          }
    Just txo -> conversionError $ "Unexpected output: " <> show txo

conversionError :: MonadError ServerError m => String -> m a
conversionError msg =
  throwError (err400{errBody=fromString msg})

valueToOutAsset :: C.Value -> [OutAsset]
valueToOutAsset =
  let  k (assetId, C.Quantity quantity) = case assetId of
            C.AdaAssetId -> OutAsset{policyId="", name="", quantity}
            C.AssetId pId aN -> OutAsset{policyId=C.serialiseToRawBytesHexText pId, name=C.serialiseToRawBytesHexText aN, quantity}

  in fmap k . C.valueToList

data OutAsset = OutAsset
  { policyId :: Text
  , name     :: Text
  , quantity :: Integer
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

