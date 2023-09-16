module Armadillo.Cli.Command(
  NodeClientConfig(..),
  localNodeConnectInfo,
  WalletClientOptions(..),
  walletClientEnv,
  Command(..),
  ServerConfig(..),
  RefScriptCommand(..),
  parseCommand
) where

import           Cardano.Api            (CardanoMode, Env, LocalNodeConnectInfo)
import qualified Cardano.Api            as C
import           Control.Monad.Except   (runExceptT)
import qualified Convex.NodeQueries     as Node
import           Convex.Wallet.Operator (OperatorConfigSigning,
                                         parseOperatorConfigSigning)
import qualified Data.Text              as Text
import qualified Network.HTTP.Client    as HTTP
import           Options.Applicative    (CommandFields, Mod, Parser, auto,
                                         command, fullDesc, help, info, long,
                                         metavar, option, progDesc, strOption,
                                         subparser, value)
import           Servant.Client         (BaseUrl (..), ClientEnv, Scheme (Http),
                                         mkClientEnv)
import           System.Exit            (exitFailure)

data NodeClientConfig =
  NodeClientConfig
    { nccCardanoNodeSocket     :: FilePath
    , nccCardanoNodeConfigFile :: FilePath
    }
    deriving stock (Eq, Ord, Show)

parseNodeClientConfig :: Parser NodeClientConfig
parseNodeClientConfig =
  NodeClientConfig
    <$> strOption (long "node.socket" <> help "Cardano node socket")
    <*> strOption (long "node.config" <> help "Cardano node config JSON file")

localNodeConnectInfo :: NodeClientConfig -> IO (LocalNodeConnectInfo CardanoMode, Env)
localNodeConnectInfo NodeClientConfig{nccCardanoNodeConfigFile, nccCardanoNodeSocket} =
  runExceptT (Node.loadConnectInfo nccCardanoNodeConfigFile nccCardanoNodeSocket) >>= \case
    Left err -> do
      putStrLn $ "localNodeConnectInfo: Failed with " <> Text.unpack (C.renderInitialLedgerStateError err)
      exitFailure
    Right x -> pure x


{-| Options for the wallet server
-}
data WalletClientOptions =
  WalletClientOptions
    { wcoHost :: String
    , wcoPort :: Int
    }
  deriving stock (Eq, Show)

walletClientEnv :: HTTP.Manager -> WalletClientOptions -> ClientEnv
walletClientEnv manager WalletClientOptions{wcoHost, wcoPort} =
  mkClientEnv manager (BaseUrl Http wcoHost wcoPort "")

data Command =
  StartServer{serverConfig :: ServerConfig } -- ^ Serve the API
  | WriteAPIFile{filePath :: FilePath } -- ^ Write the API to a file
  | RefScript NodeClientConfig RefScriptCommand

data ServerConfig =
  ServerConfig
    { scPort :: Int
    }
    deriving stock (Eq, Ord, Show)

parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ startServer
      , writeApi
      , referenceScripts
      ]

startServer :: Mod CommandFields Command
startServer = command "start-server" $
  info (StartServer <$> parseServerConfig) (fullDesc <> progDesc "Start the server")

writeApi :: Mod CommandFields Command
writeApi = command "write-api" $
  info (WriteAPIFile <$> parseAPIFile) (fullDesc <> progDesc "Write the OpenAPI3 specification of the API to a JSON file")

referenceScripts :: Mod CommandFields Command
referenceScripts = command "reference-scripts" $
  info (RefScript <$> parseNodeClientConfig <*> parseRefScriptCommand) (fullDesc <> progDesc "Manage the reference scripts")

parseServerConfig :: Parser ServerConfig
parseServerConfig =
  ServerConfig
    <$> option auto (long "http.port" <> help "Port for the HTTP server")

parseWalletClientOptions :: Parser WalletClientOptions
parseWalletClientOptions =
  WalletClientOptions
    <$> strOption (long "wallet.host" <> value "localhost" <> help "Wallet server host")
    <*> option auto (long "wallet.port" <> value 9988 <> help "Wallet server port")

parseAPIFile :: Parser FilePath
parseAPIFile =
  strOption (long "api.file" <> help "The JSON file where the API documentation should be written")

data RefScriptCommand =
  Deploy WalletClientOptions OperatorConfigSigning FilePath
  | Check

parseRefScriptCommand :: Parser RefScriptCommand
parseRefScriptCommand = subparser $ mconcat
  [ refScriptDeploy
  , refScriptCheck
  ]

refScriptDeploy :: Mod CommandFields RefScriptCommand
refScriptDeploy = command "deploy" $
  info (Deploy <$> parseWalletClientOptions <*> parseOperatorConfigSigning <*> parseOutFile) (fullDesc <> progDesc "Deploy the reference scripts")

refScriptCheck :: Mod CommandFields RefScriptCommand
refScriptCheck = command "check" $
  info (pure Check) (fullDesc <> progDesc "Check that the reference scripts are okay")

parseOutFile :: Parser FilePath
parseOutFile =
  strOption
    ( long "out.file"
    <> metavar "FILE"
    <> help "Filepath for the scripts value" )

