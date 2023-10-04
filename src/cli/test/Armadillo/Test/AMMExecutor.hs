{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-| The @amm-executor@ DEX backend as a component for integration tests
-}
module Armadillo.Test.AMMExecutor(
  RunningAMMExecutor(..),
  AMMLog(..),
  withAMMExecutor,

  -- * Other types
  ExplorerConfig(..),
  Uri(..),
  Network(..),
  ScriptsConfig(..),
  loadScriptsConfig
) where

import           Armadillo.BuildTx          (ReferenceScripts (..))
import           Armadillo.Scripts          (ScriptsConfig (..),
                                             loadScriptsConfig)
import           Cardano.Api                (ChainPoint (..), NetworkId,
                                             TxIn (..))
import qualified Cardano.Api                as C
import           Control.Concurrent.Async   (race)
import           Control.Tracer             (Tracer, traceWith)
import           Convex.Devnet.CardanoNode  (RunningNode (..))
import           Convex.Devnet.Utils        (checkProcessHasNotDied,
                                             withLogFile)
import qualified Convex.NodeQueries         as NodeQueries
import           Convex.Wallet.Operator     (OperatorConfigSigning (..),
                                             loadOperatorFiles, operatorAddress)
import           Data.Aeson                 (FromJSON (..), ToJSON (..))
import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Time.Clock            (NominalDiffTime)
import           Data.Void                  (absurd)
import           Dhall                      (FromDhall (..), Natural, ToDhall,
                                             embed, inject)
import qualified Dhall                      as D
import           Dhall.Core                 (Chunks (..), Expr (..))
import           Dhall.Pretty               (prettyExpr)
import           GHC.Generics               (Generic)
import           GHC.IO.Handle.Types        (Handle)
import           Ouroboros.Consensus.Block  (SlotNo (SlotNo))
import           Prettyprinter              (defaultLayoutOptions, layoutPretty)
import qualified Prettyprinter.Render.Text  as Render
import           Servant.API                (FromHttpApiData (..))
import           System.FilePath            ((</>))
import           System.IO                  (BufferMode (NoBuffering),
                                             hSetBuffering)
import           System.IO.Temp             (emptyTempFile, writeTempFile)
import           System.Logging.Hlog        (LogLevel (..), LoggingConfig (..))
import           System.Process             (CreateProcess (..), ProcessHandle,
                                             StdStream (UseHandle), callProcess,
                                             proc, withCreateProcess)

data RunningAMMExecutor =
  RunningAMMExecutor
    { raeHandle :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    }

data AMMLog =
  AMMLog{amlMsg :: Text }
  | AMMConfigFile{ ammConfigFile :: FilePath }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withAMMExecutor :: Tracer IO AMMLog -> FilePath -> RunningNode -> ExplorerConfig -> ReferenceScripts TxIn -> OperatorConfigSigning -> (RunningAMMExecutor -> IO a) -> IO a
withAMMExecutor tracer stateDirectory rn explorerCfg refScripts operatorConfig action = do
  configFile <- appConfig stateDirectory refScripts rn explorerCfg operatorConfig >>= writeConfigFile tracer stateDirectory
  let logFilePath = stateDirectory </> "amm-executor.log"
      process     = ammExecutorProcess (Just stateDirectory) configFile
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \stdin stdout stderr processHandle -> do
        let rae = RunningAMMExecutor{raeHandle = (stdin, stdout, stderr, processHandle)}
        fmap (either absurd id) $ race
            (checkProcessHasNotDied "amm-executor-app" processHandle)
            (action rae)

{-| Write the amm-executor-app configuration to the temp directory
and return the path to the new file
-}
writeConfigFile :: Tracer IO AMMLog -> FilePath -> AppConfig -> IO ConfigFile
writeConfigFile tracer fp cfg = do
  configFile <- writeTempFile fp "amm-executor-app-config" (toDhallString cfg)
  traceWith tracer (AMMConfigFile configFile)
  pure (ConfigFile configFile)

newtype ConfigFile = ConfigFile FilePath

ammExecutorProcess :: Maybe FilePath -> ConfigFile -> CreateProcess
ammExecutorProcess cwd (ConfigFile f) = (proc ammExecutable strAgrs){cwd} where
  strAgrs = [f]

ammExecutable :: String
ammExecutable = "amm-executor-app"

data AppConfig = AppConfig
  { nodeSocketConfig   :: !NodeSocketConfig
  , eventSourceConfig  :: !EventSourceConfig
  , ledgerStoreConfig  :: !LedgerStoreConfig
  , nodeConfigPath     :: !FilePath
  , txsInsRefs         :: !TxRefs
  , scriptsConfig      :: !ScriptsConfig
  , networkConfig      :: !NetworkConfig
  , loggingConfig      :: !LoggingConfig
  , pstoreConfig       :: !PoolStoreConfig
  , backlogConfig      :: !BacklogServiceConfig
  , backlogStoreConfig :: !BacklogStoreConfig
  , explorerConfig     :: !ExplorerConfig
  , txAssemblyConfig   :: !TxAssemblyConfig
  , secrets            :: !Secrets
  , mainnetMode        :: !Bool
  , utxoStoreConfig    :: !UtxoStoreConfig
  , unsafeEval         :: !UnsafeEvalConfig
  } deriving (Generic, ToDhall)

-- backlogConfig : …
-- , - backlogStoreConfig : …
-- , - explorerConfig : …
-- , - loggingConfig : …
-- , - mainnetMode : …
-- , - networkConfig : …
-- , - pstoreConfig : …
-- , - scriptsConfig : …
-- , - secrets : …
-- , - txAssemblyConfig : …
-- , - unsafeEval : …
-- , - utxoStoreConfig

data NodeSocketConfig = NodeSocketConfig
  { nodeSocketPath :: !FilePath
  , maxInFlight    :: !Natural
  } deriving (Generic, FromDhall, ToDhall)

appConfig :: FilePath -> ReferenceScripts TxIn -> RunningNode -> ExplorerConfig -> OperatorConfigSigning -> IO AppConfig
appConfig temp (fromRef -> txsInsRefs) RunningNode{rnConnectInfo, rnNodeSocket, rnNodeConfigFile, rnNetworkId} explorerConfig operatorConfig = do
  operator <- loadOperatorFiles operatorConfig
  startAt <- fmap fromChainPoint (NodeQueries.queryTip (fst rnConnectInfo)) >>= maybe (error "Unexpected Genesis") pure
  scriptsConfig <- loadScriptsConfig
  secrets <- opSecrets temp operatorConfig
  ammLogPath <- emptyTempFile temp "amm-executor-logs"
  let ledgerStorePath = temp </> "amm-executor-ledger-store"
      poolStorePath = temp </> "amm-executor-pool-store"
      backlogStorePath = temp </> "amm-executor-backlog-store"
      utxoStorePath = temp </> "amm-executor-utxo-store"
      createIfMissing = True
      loggingConfig = LoggingConfig{fileHandlers = [(ammLogPath, Info, "$time - $loggername - $prio - $msg")], levelOverrides = [], rootLogLevel = Info}
  pure AppConfig
        { nodeSocketConfig = NodeSocketConfig{nodeSocketPath=rnNodeSocket, maxInFlight = 10}
        , eventSourceConfig = EventSourceConfig{startAt}
        , ledgerStoreConfig = LedgerStoreConfig{storePath=ledgerStorePath, createIfMissing}
        , nodeConfigPath = rnNodeConfigFile
        , txsInsRefs
        , scriptsConfig
        , networkConfig = fromNetworkId rnNetworkId
        , loggingConfig
        , pstoreConfig = PoolStoreConfig{storePath=poolStorePath, createIfMissing}
        , backlogConfig = BacklogServiceConfig{orderExecTime = 1500, orderLifetime = 4500, suspendedPropability = 0}
        , backlogStoreConfig = BacklogStoreConfig{storePath=backlogStorePath, createIfMissing}
        , explorerConfig
        , txAssemblyConfig = TxAssemblyConfig{feePolicy=Balance, collateralPolicy=Cover, deafultChangeAddr=DefaultChangeAddress (operatorAddress rnNetworkId operator)}
        , secrets
        , mainnetMode = False
        , utxoStoreConfig = UtxoStoreConfig{utxoStorePath, createIfMissing}
        , unsafeEval = UnsafeEvalConfig{unsafeTxFee=320000, exUnits=165000000, exMem=530000}
        }

toDhallString :: AppConfig -> String
toDhallString = Text.unpack . Render.renderStrict . layoutPretty defaultLayoutOptions . prettyExpr . embed inject

data EventSourceConfig = EventSourceConfig
  { startAt :: !ConcretePoint
  } deriving (Generic, FromDhall, ToDhall)

data ConcretePoint = ConcretePoint
  { slot :: SlotNo
  , hash :: ConcreteHash
  } deriving (Generic, Eq, Show, FromDhall, ToJSON, FromJSON, ToDhall)

newtype ConcreteHash = ConcreteHash Text
  deriving newtype (Eq, Show, ToJSON, FromJSON)

instance FromDhall SlotNo where
  autoWith _ = D.Decoder{D.extract, D.expected}
    where
      extract (NaturalLit nat) = pure $ SlotNo $ fromIntegral nat
      extract expr             = D.typeError expected expr

      expected = pure Natural

instance ToDhall SlotNo where
  injectWith =
    let getSlotNo (SlotNo n) = n
    in contramap getSlotNo . D.injectWith

instance FromDhall ConcreteHash where
  autoWith _ = D.Decoder{D.extract, D.expected}
    where
      extract (TextLit (Chunks [] t)) = pure (ConcreteHash t)
      extract expr                    = D.typeError expected expr

      expected = pure Text

instance ToDhall ConcreteHash where
  injectWith = contramap concreteHashToString . D.injectWith

concreteHashToString :: ConcreteHash -> D.Text
concreteHashToString (ConcreteHash h) = h

fromChainPoint :: ChainPoint -> Maybe ConcretePoint
fromChainPoint C.ChainPointAtGenesis = Nothing
fromChainPoint (C.ChainPoint sn hsh) = Just (ConcretePoint sn $ ConcreteHash $ C.serialiseToRawBytesHexText hsh)

data LedgerStoreConfig = LedgerStoreConfig
  { storePath       :: !FilePath
  , createIfMissing :: !Bool
  }
  deriving (Generic, FromDhall, ToDhall)

data TxRefs = TxRefs
  { swapRef    :: !TxIn
  , depositRef :: !TxIn
  , redeemRef  :: !TxIn
  , poolV1Ref  :: !TxIn
  , poolV2Ref  :: !TxIn
  } deriving (Generic, ToDhall)

instance ToDhall TxIn where
  injectWith =
    let getText (TxIn a (C.TxIx b)) = C.serialiseToRawBytesHexText a <> "#" <> Text.pack (show b)
    in contramap getText . D.injectWith

fromRef :: ReferenceScripts TxIn -> TxRefs
fromRef ReferenceScripts{refScriptPool, refScriptSwap, refScriptDeposit, refScriptRedeem} =
  TxRefs
    { swapRef = refScriptSwap
    , depositRef = refScriptDeposit
    , redeemRef = refScriptRedeem
    , poolV1Ref = refScriptPool
    , poolV2Ref = refScriptPool
    }

data NetworkConfig = NetworkConfig
  { cardanoNetworkId :: !Natural
  } deriving (Generic, ToDhall)

fromNetworkId :: NetworkId -> NetworkConfig
fromNetworkId = \case
  C.Mainnet ->
    -- From config.dhall: "In mainnet mode, the network magic ID is not utilized."
    -- so we can return an arbitrary ID
    NetworkConfig 2
  C.Testnet (C.NetworkMagic n) -> NetworkConfig (fromIntegral n)

deriving anyclass instance ToDhall LoggingConfig

deriving anyclass instance ToDhall LogLevel

deriving anyclass instance ToDhall (FilePath, LogLevel, String)

data PoolStoreConfig = PoolStoreConfig
  { storePath       :: !FilePath
  , createIfMissing :: !Bool
  }
  deriving (Generic, ToDhall)

data BacklogServiceConfig = BacklogServiceConfig
  { orderLifetime        :: !NominalDiffTime
  , orderExecTime        :: !NominalDiffTime
  , suspendedPropability :: !Natural
  } deriving (Generic, ToDhall, Show)


instance ToDhall NominalDiffTime where
  injectWith = contramap round . D.injectWith @Natural -- ?

data BacklogStoreConfig = BacklogStoreConfig
  { storePath       :: !FilePath
  , createIfMissing :: !Bool
  }
  deriving (Generic, ToDhall)

newtype Uri = Uri { unUri :: String }
  deriving Generic
  deriving newtype (Show, ToDhall)

data Network = Mainnet | Preview
  deriving (Generic, Show, ToDhall)
  deriving anyclass (ToJSON, FromJSON)

instance FromHttpApiData Network where
  parseUrlPiece = \case
    "mainnet" -> pure Mainnet
    "preview" -> pure Preview
    k -> Left ("unknown network: " <> k)

data ExplorerConfig = ExplorerConfig
  { explorerUri :: Uri
  , network     :: Network
  } deriving (Generic, Show, ToDhall)

data FeePolicy
  = Strict  -- Require existing TX inputs to cover fee entirely
  | Balance -- Allow adding new inputs to cover fee
  deriving stock Generic
  deriving anyclass ToDhall

instance D.FromDhall FeePolicy

data CollateralPolicy
  = Ignore -- Ignore collateral inputs
  | Cover  -- Allow adding new inputs to cover collateral
  deriving stock Generic
  deriving anyclass ToDhall

instance D.FromDhall CollateralPolicy

data TxAssemblyConfig = TxAssemblyConfig
  { feePolicy         :: FeePolicy
  , collateralPolicy  :: CollateralPolicy
  , deafultChangeAddr :: DefaultChangeAddress
  } deriving stock Generic
    deriving anyclass ToDhall

newtype DefaultChangeAddress = DefaultChangeAddress { getChangeAddr :: C.Address C.ShelleyAddr }

instance ToDhall DefaultChangeAddress where
  injectWith = contramap (C.serialiseAddress . getChangeAddr) . D.injectWith

data Secrets = Secrets
  { secretFile :: !SecretFile
  , keyPass    :: !KeyPass
  } deriving stock (Generic)
    deriving anyclass ToDhall

newtype SecretFile = SecretFile { unSigningKeyFile :: FilePath }
  deriving (Show, Eq, Generic)
  deriving newtype ToDhall

newtype KeyPass = KeyPass { unKeyPass :: Text }
  deriving (Show, Eq, Generic)
  deriving newtype ToDhall

data UtxoStoreConfig = UtxoStoreConfig
   { utxoStorePath   :: FilePath
   , createIfMissing :: Bool
   } deriving stock (Generic)
     deriving anyclass (ToDhall)

data UnsafeEvalConfig = UnsafeEvalConfig
  { unsafeTxFee :: Integer
  , exUnits     :: Natural
  , exMem       :: Natural
  } deriving stock Generic
    deriving anyclass ToDhall

opSecrets :: FilePath -> OperatorConfigSigning -> IO Secrets
opSecrets temp OperatorConfigSigning{ocSigningKeyFile} = do
  containerPath <- emptyTempFile temp "trust-store"
  let pass = "password"
  callProcess "wallet-helper-app" [containerPath, ocSigningKeyFile, pass]
  pure $
    Secrets
      { secretFile=SecretFile containerPath
      , keyPass=KeyPass (Text.pack pass)
      }
-- needs https://github.com/ergolabs/cardano-explorer.git
-- to be running
