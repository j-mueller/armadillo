{-# LANGUAGE OverloadedStrings #-}
{-| CLI
-}
module Armadillo.Cli(
  runCli,
  writeJSONFile,
  readJSONFile
  ) where

import qualified Armadillo.Api              as Api
import           Armadillo.BuildTx          (PoolOutput (..))
import           Armadillo.Cli.Command      (Command (..), DebugCommand (..),
                                             Fee (..), PoolCommand (..),
                                             RefScriptCommand (..),
                                             ServerConfig (..), apiClientEnv,
                                             localNodeConnectInfo, parseCommand,
                                             walletClientEnv)
import           Armadillo.Command          (CreatePoolParams (..))
import qualified Armadillo.Command          as Command
import qualified Armadillo.NodeClient       as NodeClient
import           Armadillo.Scripts          (loadScriptsConfig,
                                             scriptsFromScriptsConfig)
import qualified Armadillo.Server           as Server
import           Armadillo.Utils            (readJSONFile)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Convex.MonadLog            (runMonadLogKatip, withKatipLogging)
import           Convex.Wallet.Operator     (loadOperatorFiles)
import           Data.Aeson                 (ToJSON)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Foldable              (traverse_)
import           Data.String                (IsString (..))
import qualified Katip                      as K
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Options.Applicative        (customExecParser, disambiguate,
                                             helper, idm, info, prefs,
                                             showHelpOnEmpty, showHelpOnError)
import           System.Exit                (exitFailure)

runCli :: IO ()
runCli = do
  withKatipLogging K.InfoS "armadillo" "cli" $ \config -> do
    command <- liftIO (customExecParser
                      (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                      (info (helper <*> parseCommand) idm))
    scripts <- loadScriptsConfig >>= scriptsFromScriptsConfig >>= either (error "scriptsFromScriptsConfig failed") pure
    case command of
      StartServer{serverConfig, nodeClientConfig} -> do
        putStrLn $ "Starting server on port " <> show (scPort serverConfig)
        let (katipConfig, _, _) = config
        stateVar <- traverse (\(cfg, stateFile, chainPoints) -> NodeClient.runArmadilloNodeClient scripts katipConfig cfg stateFile chainPoints) nodeClientConfig
        Server.runServer stateVar serverConfig
      WriteAPIFile{filePath} -> do
        putStrLn $ "Writing API to " <> filePath
        Api.writeApiToFile filePath
      RefScript nodeConfig com -> do
        (connectInfo, nodeEnv) <- localNodeConnectInfo nodeConfig
        case com of
          Deploy walletClient opConfig outFile -> do
            walletEnv <- walletClientEnv <$> mgr <*> pure walletClient
            op <- loadOperatorFiles opConfig
            x <- runMonadLogKatip config (Command.runBlockchainAction connectInfo nodeEnv walletEnv (Command.deployRefScripts scripts op))
            case x of
              Left err -> do
                putStrLn (show err)
                exitFailure
              Right k -> do
                liftIO $ LBS.writeFile outFile $ encodePretty k
                putStrLn (show k)
          Check{} -> do
            putStrLn "Not implemented: reference-scripts check"
            exitFailure
      Pool nodeConfig outFile com -> do
        (connectInfo, nodeEnv) <- localNodeConnectInfo nodeConfig
        case com of
          Create walletClient operatorConfig (Fee cppFee) cppAssetClassX cppAssetClassY -> do
            op <- loadOperatorFiles operatorConfig
            walletEnv <- walletClientEnv <$> mgr <*> pure walletClient
            let params =
                  CreatePoolParams
                    { cppOperator = op
                    , cppFee
                    , cppAssetClassX
                    , cppAssetClassY
                    }
            result <- runMonadLogKatip config (Command.runBlockchainAction connectInfo nodeEnv walletEnv (Command.createPool scripts params))
            case result of
              Left err -> do
                putStrLn (show err)
                exitFailure
              Right k -> traverse_ (\x' -> writeJSONFile x' k) outFile
          Deposit walletClient operatorConfig apiClientOptions assetClassX assetClassY quantity -> do
            op <- loadOperatorFiles operatorConfig
            m <- mgr
            let walletEnv = walletClientEnv m walletClient
                apiEnv    = apiClientEnv m apiClientOptions
                isMatchingPool _ = True -- FIXME
            pools <- Api.getPoolOutputs apiEnv >>= either (error . show . (<>) "getPoolOutputs failed: " . show) pure
            case filter isMatchingPool pools of
              [] -> error $ "No pool found for " <> show (assetClassX, assetClassY)
              PoolOutput{poConfig}:_ -> do
                result <- runMonadLogKatip config (Command.runBlockchainAction connectInfo nodeEnv walletEnv $ Command.makeDeposit scripts op poConfig quantity)
                case result of
                  Left err -> do
                    putStrLn (show err)
                    exitFailure
                  Right tx ->
                    traverse_ (\x' -> writeJSONFile x' tx) outFile
      Debug nodeConfig outFile com -> do
        (connectInfo, nodeEnv) <- localNodeConnectInfo nodeConfig
        case com of
            CreateCurrency walletClient operatorConfig tokenName -> do
              putStrLn "Creating new currency"
              op <- loadOperatorFiles operatorConfig
              walletEnv <- walletClientEnv <$> mgr <*> pure walletClient
              x <- runMonadLogKatip config (Command.runBlockchainAction connectInfo nodeEnv walletEnv (Command.createToken op (fromString tokenName) 1000))
              case x of
                Left err -> do
                  putStrLn (show err)
                  exitFailure
                Right (_, scriptHash) ->
                  traverse_ (\x' -> writeJSONFile x' scriptHash) outFile

mgr :: IO Manager
mgr = newManager defaultManagerSettings

{-| Encode a value as JSON and write it to a file
-}
writeJSONFile :: ToJSON a => FilePath -> a -> IO ()
writeJSONFile file = BSL.writeFile file . encodePretty
