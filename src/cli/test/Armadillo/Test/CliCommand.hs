{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
module Armadillo.Test.CliCommand(
  RunningCliProcess(..),
  withCliCommand,
  runCliCommand,
  CliLog(..),

  -- * Running the HTTP server
  RunningHttpServer(..),
  withHttpServer,
  apiHealth
) where

import qualified Armadillo.Api               as Api
import           Armadillo.Cli.Command       (Command (..),
                                              RefScriptCommand (..),
                                              ServerConfig (..),
                                              WalletClientOptions (..))
import           Control.Concurrent          (threadDelay)
import           Control.Monad               (void)
import           Control.Tracer              (Tracer, traceWith)
import           Convex.Devnet.Utils         (failure, withLogFile)
import           Convex.Wallet.Operator      (OperatorConfigSigning (..))
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           GHC.Generics                (Generic)
import           GHC.IO.Exception            (ExitCode (ExitSuccess))
import           GHC.IO.Handle.Types         (Handle)
import           Network.HTTP.Client         (defaultManagerSettings,
                                              newManager)
import           Servant.Client              (ClientEnv, mkClientEnv)
import           Servant.Client.Core.BaseUrl (BaseUrl (..), Scheme (..))
import           System.FilePath             ((</>))
import           System.IO                   (BufferMode (NoBuffering),
                                              hSetBuffering)
import           System.Process              (CreateProcess (..), ProcessHandle,
                                              StdStream (UseHandle), proc,
                                              waitForProcess, withCreateProcess)

data CliLog =
  MsgText{ msgText :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

commandString :: Command -> String
commandString = \case
  StartServer{} -> "start-server"
  WriteAPIFile{} -> "write-api"
  RefScript{} -> "reference-scripts"

cliProcess :: Maybe FilePath -> Command -> CreateProcess
cliProcess cwd command = (proc cliExecutable strArgs){cwd} where
  serverPort = \case
    StartServer ServerConfig{scPort} -> ["--http.port", show scPort]
    _ -> []
  apiFile = \case
    WriteAPIFile{filePath} -> ["--api.file", filePath]
    _ -> []

  refCommand = \case
    RefScript c -> case c of
      Deploy{} -> ["deploy"]
      Check{}  -> ["check"]
    _ -> []

  walletClientOptions' = \case
    RefScript (Deploy wco _) -> walletClientOptions wco
    _ -> []

  operatorSigningConfig' = \case
    RefScript (Deploy _ osc) -> operatorSigningConfig osc
    _ -> []

  strArgs =
    mconcat
      [ [commandString command]
      , serverPort command
      , apiFile command
      , refCommand command
      , walletClientOptions' command
      , operatorSigningConfig' command
      , ["+RTS", "-N2"]
      ]

walletClientOptions :: WalletClientOptions -> [String]
walletClientOptions WalletClientOptions{wcoHost, wcoPort} =
  [ "--wallet.host", wcoHost
  , "--wallet.port", show wcoPort
  ]

operatorSigningConfig :: OperatorConfigSigning -> [String]
operatorSigningConfig OperatorConfigSigning{ocSigningKeyFile, ocStakeVerificationKeyFile} =
  ["--signing-key-file", ocSigningKeyFile]
  ++ maybe [] (\f -> ["--stake-verification-key-file", f]) ocStakeVerificationKeyFile

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
apiHealth RunningHttpServer{rhClient} =
  void (Api.getHealth rhClient >>= either (fail . show) pure)
