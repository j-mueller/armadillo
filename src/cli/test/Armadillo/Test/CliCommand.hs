{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
module Armadillo.Test.CliCommand(
  -- * armadillo cli
  RunningCliProcess(..),
  withCliCommand,
  runCliCommand,

  -- ** CLI Commands
  createCurrency,
  createPool,

  -- * Running the HTTP server
  RunningHttpServer(..),
  withHttpServer,
  apiHealth,
  apiPairs,
  apiTransactions

) where

import           Armadillo.Api               (Pair, PairID, Transaction)
import qualified Armadillo.Api               as Api
import           Armadillo.Cli               (readJSONFile)
import           Armadillo.Cli.Command       (Command (..), DebugCommand (..),
                                              Fee (..), NodeClientConfig (..),
                                              PoolCommand (..),
                                              RefScriptCommand (..),
                                              ServerConfig (..),
                                              WalletClientOptions (..),
                                              unReadAssetId)
import           Armadillo.Command           (ActivePool (..))
import           Armadillo.Test.DevEnv       (CliLog (..), DevEnv (..),
                                              TestLog (..))
import           Armadillo.Test.Utils        (nodeClientConfig)
import           Cardano.Api                 (AssetId)
import qualified Cardano.Api                 as C
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (void)
import           Control.Tracer              (Tracer, contramap, traceWith)
import           Convex.Devnet.Utils         (failure, withLogFile)
import           Convex.Devnet.WalletServer  (RunningWalletServer (..))
import           Convex.Wallet.Operator      (OperatorConfigSigning (..))
import           Data.String                 (IsString (..))
import qualified Data.Text                   as Text
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
      _ -> failure $ "runCliCommand: " <> commandString command <> " exits with failure code " <> show result

createCurrency :: DevEnv -> String -> IO AssetId
createCurrency DevEnv{tempDir, wallet=RunningWalletServer{rwsOpConfigSigning}, node, tracer, walletClientOptions} name = do
  outFile <- emptyTempFile tempDir "script-hash"
  runCliCommand (contramap TCli tracer) tempDir (Debug (nodeClientConfig node) (Just outFile) $ CreateCurrency walletClientOptions rwsOpConfigSigning name)
  scriptHash <- readJSONFile outFile >>= maybe (error $ "Unable to read JSON file " <> outFile) pure
  pure $ C.AssetId (C.PolicyId scriptHash) (fromString name)

createPool :: DevEnv -> Fee -> AssetId -> AssetId -> IO ActivePool
createPool DevEnv{tempDir, wallet=RunningWalletServer{rwsOpConfigSigning}, node, tracer, walletClientOptions} fee assetX assetY = do
  outFile <- emptyTempFile tempDir "active-pool"
  runCliCommand (contramap TCli tracer) tempDir (Pool (nodeClientConfig node) (Just outFile) $ Create walletClientOptions rwsOpConfigSigning fee (assetX, 50) (assetY, 50))
  readJSONFile outFile >>= maybe (error $ "Unable to read JSON file " <> outFile) pure

commandString :: Command -> String
commandString = \case
  StartServer{} -> "start-server"
  WriteAPIFile{} -> "write-api"
  RefScript{} -> "reference-scripts"
  Pool{} -> "pool"
  Debug{} -> "debug"

cliProcess :: Maybe FilePath -> Command -> CreateProcess
cliProcess cwd command = (proc cliExecutable strArgs){cwd} where
  serverPort = \case
    StartServer ServerConfig{scPort} -> ["--http.port", show scPort]
    _ -> []

  apiFile = \case
    WriteAPIFile{filePath} -> ["--api.file", filePath]
    _ -> []

  refCommand = \case
    RefScript _ c -> case c of
      Deploy{} -> ["deploy"]
      Check{}  -> ["check"]
    _ -> []

  poolCom = \case
    Pool _ _ c -> case c of
      Create{} -> ["create"]
    _ -> []

  debugCom = \case
    Debug _ _ c -> case c of
      CreateCurrency{} -> ["create-currency"]
    _ -> []

  nodeClient = \case
    RefScript cfg _ -> nodeClientConfigArgs cfg
    Pool cfg _ _ -> nodeClientConfigArgs cfg
    Debug cfg _ _ -> nodeClientConfigArgs cfg
    _ -> []

  walletClientOptions' = \case
    RefScript _ (Deploy wco _ _) -> walletClientOptionsCfg wco
    Debug _ _ (CreateCurrency wco _ _) -> walletClientOptionsCfg wco
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
    Pool _ (Just f) _ -> ["--out.file", f]
    _ -> []

  outFile = \case
    RefScript _ (Deploy _ _ f) -> ["--out.file", f]
    _ -> []

  poolArgs = \case
    Pool _ _ com -> case com of
      Create walletClient ocf (Fee n) pairX pairY ->
        walletClientOptionsCfg walletClient
        ++ operatorSigningConfig ocf
        ++ ["--pool.fee", show n]
        ++ ["--pool.x.assetID", unReadAssetId (fst pairX)]
        ++ ["--pool.y.assetID", unReadAssetId (fst pairY)]
    _ -> []

  strArgs =
    mconcat
      [ [commandString command]
      , serverPort command
      , apiFile command
      , nodeClient command
      , debugOutFile command
      , refCommand command
      , poolCom command
      , debugCom command
      , walletClientOptions' command
      , operatorSigningConfig' command
      , outFile command
      , tokenName command
      , poolArgs command
      , ["+RTS", "-N2"]
      ]

walletClientOptionsCfg :: WalletClientOptions -> [String]
walletClientOptionsCfg WalletClientOptions{wcoHost, wcoPort} =
  [ "--wallet.host", wcoHost
  , "--wallet.port", show wcoPort
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

{-| Start the armadillo HTTP server
-}
withHttpServer :: Tracer IO CliLog -> FilePath -> ServerConfig -> (RunningHttpServer -> IO a) -> IO a
withHttpServer tracer stateDirectory cfg action =
  let command = StartServer cfg
  in withCliCommand tracer stateDirectory command $ \rhProcess -> do
    rhClient <- mkClientEnv <$> newManager defaultManagerSettings <*> pure (BaseUrl Http "localhost" (scPort cfg) "")
    threadDelay 2_000_000
    action RunningHttpServer{rhClient, rhProcess}

apiHealth :: RunningHttpServer -> IO ()
apiHealth = void . runApiCall Api.getHealth

apiPairs :: RunningHttpServer -> IO [Pair]
apiPairs = runApiCall Api.getPairs

apiTransactions :: RunningHttpServer -> PairID -> IO [Transaction]
apiTransactions server pair = runApiCall (\k -> Api.getTransactions k Nothing pair) server

runApiCall :: (ClientEnv -> IO (Either ClientError a)) -> RunningHttpServer -> IO a
runApiCall call RunningHttpServer{rhClient} = call rhClient >>= either (fail . show) pure
