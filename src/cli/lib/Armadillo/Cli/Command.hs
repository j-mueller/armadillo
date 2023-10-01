{-# LANGUAGE OverloadedStrings #-}
module Armadillo.Cli.Command(
  NodeClientConfig(..),
  localNodeConnectInfo,
  WalletClientOptions(..),
  walletClientEnv,
  Command(..),
  ServerConfig(..),
  RefScriptCommand(..),
  PoolCommand(..),
  Fee(..),
  parseCommand,
  DebugCommand(..),

  -- * Internals exposed for debuggung
  readAssetId,
  unReadAssetId
) where

import           Cardano.Api            (AssetId, CardanoMode, Env,
                                         LocalNodeConnectInfo, Quantity (..))
import qualified Cardano.Api            as C
import           Control.Monad.Except   (runExceptT)
import qualified Convex.NodeQueries     as Node
import           Convex.Wallet.Operator (OperatorConfigSigning,
                                         parseOperatorConfigSigning)
import           Data.Bifunctor         (Bifunctor (..))
import           Data.Proxy             (Proxy (..))
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Network.HTTP.Client    as HTTP
import           Options.Applicative    (CommandFields, Mod, Parser, auto,
                                         command, eitherReader, fullDesc, help,
                                         info, long, metavar, option, optional,
                                         progDesc, strOption, subparser, value)
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
  | Pool NodeClientConfig (Maybe FilePath) PoolCommand
  | Debug NodeClientConfig (Maybe FilePath) DebugCommand -- ^ Debug command with an output file

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
      , poolCom
      , debugCom
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

newtype Fee = Fee Integer
  deriving stock (Eq, Ord, Show)

parseFee :: Parser Fee
parseFee = fmap Fee $ option auto (long "pool.fee" <> value 100 <> help "Pool fee")

data PoolCommand =
  Create WalletClientOptions OperatorConfigSigning Fee (AssetId, Quantity) (AssetId, Quantity)
  deriving stock (Eq, Show)

parseAssetId :: String -> Parser AssetId
parseAssetId x = option (eitherReader readAssetId) (long ("pool." <> x <> ".assetID") <> help "Asset ID")

parseQuantity :: Parser Quantity
parseQuantity = pure (Quantity 50) -- FIXME

parseAssetIdQuantityPair :: String -> Parser (AssetId, Quantity)
parseAssetIdQuantityPair x = (,) <$> parseAssetId x <*> parseQuantity

poolCom :: Mod CommandFields Command
poolCom = command "pool" $
  info (Pool <$> parseNodeClientConfig <*> optional parseDebugOutFile <*> parsePoolCommand) (fullDesc <> progDesc "Manage liquidity pools")

parsePoolCommand :: Parser PoolCommand
parsePoolCommand = subparser poolCreate

poolCreate :: Mod CommandFields PoolCommand
poolCreate = command "create" $ info (Create <$> parseWalletClientOptions <*> parseOperatorConfigSigning <*> parseFee <*> parseAssetIdQuantityPair "x" <*> parseAssetIdQuantityPair "y") (fullDesc <> progDesc "Create a new pool")

data DebugCommand =
  CreateCurrency WalletClientOptions OperatorConfigSigning String -- ^ Create a currency with the given name

debugCom :: Mod CommandFields Command
debugCom = command "debug" $
  info (Debug <$> parseNodeClientConfig <*> optional parseDebugOutFile <*> subparser createCurrency) (fullDesc <> progDesc "Debugging tools")

createCurrency :: Mod CommandFields DebugCommand
createCurrency = command "create-currency" $ info (CreateCurrency <$> parseWalletClientOptions <*> parseOperatorConfigSigning <*> parseCurrencyName) (fullDesc <> progDesc "The name of the currency")

parseCurrencyName :: Parser String
parseCurrencyName =
  strOption (long "currency.name" <> help "Token name of the new currency")

parseDebugOutFile :: Parser FilePath
parseDebugOutFile =
  strOption
    ( long "out.file"
    <> metavar "FILE"
    <> help "Filepath for the output" )

readAssetId :: String -> Either String AssetId
readAssetId = \case
  "ada" -> Right C.AdaAssetId
  x -> case splitAt 56 x of
        (policyId, ':' : assetName) ->
          C.AssetId
            <$> (first (\err -> "Failed to decode policy ID: " <> show err) $ C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy) (Text.encodeUtf8 $ Text.pack policyId))
            <*> (first (\err -> "Failed to decode asset name: " <> show err) $ C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy) (Text.encodeUtf8 $ Text.pack assetName))
        _ -> Left "Unable to decode asset ID. Expected <56-character hex encoded policy ID>:<hex-encoded asset name>"

unReadAssetId :: AssetId -> String
unReadAssetId = \case
  C.AdaAssetId -> "ada"
  C.AssetId policyId assetName ->
    Text.unpack $
      C.serialiseToRawBytesHexText policyId <> ":" <> C.serialiseToRawBytesHexText assetName
