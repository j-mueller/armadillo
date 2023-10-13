{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
module Armadillo.Test.CliCommand(
  -- * armadillo cli
  CliLog(..),
  RunningCliProcess(..),
  withCliCommand,
  runCliCommand,
  mkNodeClientConfig,

  -- ** CLI Commands
  deployScripts,
  loadRefScripts,

  -- * Running the HTTP server
  RunningHttpServer(..),
  ChainFollowerStartup(..),
  withHttpServer,
  apiHealth,
  apiPairs,
  apiTransactions,
  apiDeposits,
  apiPools
) where

import           Armadillo.Api               (Pair, PairID, Transaction)
import qualified Armadillo.Api               as Api
import           Armadillo.BuildTx           (DepositOutput, PoolOutput,
                                              ReferenceScripts (..))
import           Armadillo.Cli.Command       (Command (..), DebugCommand (..),
                                              NodeClientConfig (..),
                                              NodeClientStateFile (..),
                                              RefScriptCommand (..),
                                              ServerConfig (..))
import           Armadillo.Kupo              (KupoConfig (..))
import           Cardano.Api                 (ChainPoint, TxIn)
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (void)
import           Control.Tracer              (Tracer, traceWith)
import           Convex.Devnet.CardanoNode   (RunningNode (..))
import           Convex.Devnet.Utils         (failure, withLogFile)
import           Convex.Wallet.Operator      (OperatorConfigSigning (..))
import           Data.Aeson                  (FromJSON, ToJSON, decode)
import qualified Data.ByteString.Lazy        as BSL
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           GHC.Generics                (Generic)
import           GHC.IO.Exception            (ExitCode (ExitSuccess))
import           GHC.IO.Handle.Types         (Handle)
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Servant.Client              (ClientEnv, ClientError,
                                              mkClientEnv)
import           Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..))
import           System.FilePath             ((</>))
import           System.IO                   (BufferMode (NoBuffering),
                                              hSetBuffering)
import           System.IO.Temp              (emptyTempFile)
import           System.Process              (CreateProcess (..), ProcessHandle,
                                              StdStream (UseHandle), proc,
                                              waitForProcess, withCreateProcess)

data RunningCliProcess =
  RunningCliProcess
    { rcpCommand :: Command
    , rcpHandle  :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    }

data CliLog =
  MsgText{ msgText :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

withCliCommand :: Tracer IO CliLog -> FilePath -> Command -> (RunningCliProcess -> IO a) -> IO a
withCliCommand tracer stateDirectory command action = do
  let logFilePath = stateDirectory </> commandString command <> ".log"
      process = cliProcess (Just stateDirectory) command
  traceWith tracer $ MsgText{msgText = Text.pack (show $ cmdspec process) }
  withLogFile logFilePath $ \out -> do
    hSetBuffering out NoBuffering
    withCreateProcess process{std_out = UseHandle out, std_err = UseHandle out} $
      \stdin stdout stderr processHandle -> do
        let p = RunningCliProcess{rcpCommand = command, rcpHandle = (stdin, stdout, stderr, processHandle)}
        action p

{-| Run a cli command, wait for the process to exit successfully
-}
runCliCommand :: Tracer IO CliLog -> FilePath -> Command -> IO ()
runCliCommand tracer stateDirectory command =
  withCliCommand tracer stateDirectory command $ \RunningCliProcess{rcpHandle = (_, _, _, processHandle)} -> do
    result <- waitForProcess processHandle
    case result of
      ExitSuccess -> pure ()
      _ -> do
        failure $ "runCliCommand: " <> commandString command <> " exits with failure code " <> show result

deployScripts :: Tracer IO CliLog -> FilePath -> RunningNode -> KupoConfig -> OperatorConfigSigning -> IO (ReferenceScripts TxIn)
deployScripts tracer tempDir node kupoConfig opConfigSigning = do
  let outFile = tempDir </> "reference-scripts.json"
  runCliCommand tracer tempDir (RefScript (mkNodeClientConfig node) $ Deploy kupoConfig opConfigSigning outFile)
  loadRefScripts outFile

loadRefScripts :: FilePath -> IO (ReferenceScripts TxIn)
loadRefScripts fp = decode <$> BSL.readFile fp >>= maybe (error $ "loadRefScripts: failed to load file " <> fp) pure

commandString :: Command -> String
commandString = \case
  StartServer{} -> "start-server"
  WriteAPIFile{} -> "write-api"
  RefScript{} -> "reference-scripts"
  Debug{} -> "debug"

cliProcess :: Maybe FilePath -> Command -> CreateProcess
cliProcess cwd command = (proc cliExecutable strArgs){cwd} where
  serverPort = \case
    StartServer ServerConfig{scPort} _ -> ["--http.port", show scPort]
    _ -> []

  chainFollowerConf = \case
    StartServer _ (Just (nodeClientConfig, NodeClientStateFile f, KupoConfig{kcHost, kcPort}, [])) ->
      nodeClientConfigArgs nodeClientConfig
      ++ ["--kupo.host", kcHost]
      ++ ["--kupo.port", show kcPort]
      ++ ["--node-client.file", f]
    StartServer _ (Just (_, _, _, _cp)) -> error "chainFollowerConf: not supported: ChainPoints"
    _ -> []

  apiFile = \case
    WriteAPIFile{filePath} -> ["--api.file", filePath]
    _ -> []

  refCommand = \case
    RefScript _ c -> case c of
      Deploy{} -> ["deploy"]
      Check{}  -> ["check"]
    _ -> []

  debugCom = \case
    Debug _ _ c -> case c of
      CreateCurrency{} -> ["create-currency"]
    _ -> []

  nodeClient = \case
    RefScript cfg _ -> nodeClientConfigArgs cfg
    Debug cfg _ _ -> nodeClientConfigArgs cfg
    _ -> []

  kupoConf = \case
    RefScript _ (Deploy wco _ _) -> kupoConfigOptions wco
    Debug _ _ (CreateCurrency wco _ _) -> kupoConfigOptions wco
    _ -> []

  operatorSigningConfig' = \case
    RefScript _ (Deploy _ osc _) -> operatorSigningConfig osc
    Debug _ _ (CreateCurrency _ osc _) -> operatorSigningConfig osc
    _ -> []

  tokenName = \case
    Debug _ _ (CreateCurrency _ _ tn) -> ["--currency.name", tn]
    _ -> []

  debugOutFile = \case
    Debug _ (Just f) _ -> ["--out.file", f]
    _ -> []

  outFile = \case
    RefScript _ (Deploy _ _ f) -> ["--out.file", f]
    _ -> []

  strArgs =
    mconcat
      [ [commandString command]
      , serverPort command
      , chainFollowerConf command
      , apiFile command
      , nodeClient command
      , debugOutFile command
      , refCommand command
      , debugCom command
      , kupoConf command
      , operatorSigningConfig' command
      , outFile command
      , tokenName command
      , ["+RTS", "-N2"]
      ]

operatorSigningConfig :: OperatorConfigSigning -> [String]
operatorSigningConfig OperatorConfigSigning{ocSigningKeyFile, ocStakeVerificationKeyFile} =
  ["--signing-key-file", ocSigningKeyFile]
  ++ maybe [] (\f -> ["--stake-verification-key-file", f]) ocStakeVerificationKeyFile

nodeClientConfigArgs :: NodeClientConfig -> [String]
nodeClientConfigArgs NodeClientConfig{nccCardanoNodeSocket, nccCardanoNodeConfigFile} =
  [ "--node.socket", nccCardanoNodeSocket
  , "--node.config", nccCardanoNodeConfigFile
  ]

cliExecutable :: String
cliExecutable = "armadillo-cli"

data RunningHttpServer =
  RunningHttpServer
    { rhProcess :: RunningCliProcess
    , rhClient  :: ClientEnv
    }

{-| Whether to start the chain follower alongside the HTTP server
-}
data ChainFollowerStartup =
  StartChainFollower FilePath RunningNode KupoConfig
  | DontStartChainFollower

chainFollowerConfig :: ChainFollowerStartup -> IO (Maybe (NodeClientConfig, NodeClientStateFile, KupoConfig, [ChainPoint]))
chainFollowerConfig = \case
  DontStartChainFollower -> pure Nothing
  StartChainFollower fp rn k -> do
    f <- NodeClientStateFile <$> emptyTempFile fp "node-client-state"
    pure (Just (mkNodeClientConfig rn, f, k, []))

{-| Start the armadillo HTTP server
-}
withHttpServer :: Tracer IO CliLog -> FilePath -> ServerConfig -> ChainFollowerStartup -> (RunningHttpServer -> IO a) -> IO a
withHttpServer tracer stateDirectory cfg cfs action = do
  command <- StartServer cfg <$> chainFollowerConfig cfs
  withCliCommand tracer stateDirectory command $ \rhProcess -> do
    rhClient <- mkClientEnv <$> newManager defaultManagerSettings <*> pure (BaseUrl Http "localhost" (scPort cfg) "")
    threadDelay 2_000_000
    action RunningHttpServer{rhClient, rhProcess}

apiHealth :: RunningHttpServer -> IO ()
apiHealth = void . runApiCall Api.getHealth

apiPairs :: RunningHttpServer -> IO [Pair]
apiPairs = runApiCall Api.getPairs

apiTransactions :: RunningHttpServer -> PairID -> IO [Transaction]
apiTransactions server pair = runApiCall (\k -> Api.getTransactions k Nothing pair) server

apiDeposits :: RunningHttpServer -> IO [DepositOutput TxIn]
apiDeposits = runApiCall Api.getDepositOutputs

apiPools :: RunningHttpServer -> IO [PoolOutput TxIn]
apiPools = runApiCall Api.getPoolOutputs

runApiCall :: (ClientEnv -> IO (Either ClientError a)) -> RunningHttpServer -> IO a
runApiCall call RunningHttpServer{rhClient} = call rhClient >>= either (fail . show) pure

mkNodeClientConfig :: RunningNode -> NodeClientConfig
mkNodeClientConfig RunningNode{rnNodeConfigFile, rnNodeSocket} =
  NodeClientConfig
    { nccCardanoNodeSocket = rnNodeSocket
    , nccCardanoNodeConfigFile = rnNodeConfigFile
    }

kupoConfigOptions :: KupoConfig -> [String]
kupoConfigOptions KupoConfig{kcHost, kcPort} =
  [ "--kupo.host", kcHost
  , "--kupo.port", show kcPort
  ]
