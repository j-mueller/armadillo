{-# LANGUAGE OverloadedStrings #-}
{-| CLI
-}
module Armadillo.Cli(
  runCli,
  writeJSONFile,
  readJSONFile
  ) where

import qualified Armadillo.Api              as Api
import           Armadillo.Cli.Command      (Command (..), DebugCommand (..),
                                             Fee (..), PoolCommand (..),
                                             RefScriptCommand (..),
                                             ServerConfig (..),
                                             localNodeConnectInfo, parseCommand,
                                             walletClientEnv)
import           Armadillo.Command          (CreatePoolParams (..))
import qualified Armadillo.Command          as Command
import qualified Armadillo.Server           as Server
import           Control.Exception          (SomeException, catch)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Convex.MonadLog            (runMonadLogKatip, withKatipLogging)
import           Convex.Wallet.Operator     (loadOperatorFiles)
import           Data.Aeson                 (FromJSON, ToJSON, decode)
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
    case command of
      StartServer{serverConfig} -> do
        putStrLn $ "Starting server on port " <> show (scPort serverConfig)
        Server.runServer serverConfig
      WriteAPIFile{filePath} -> do
        putStrLn $ "Writing API to " <> filePath
        Api.writeApiToFile filePath
      RefScript nodeConfig com -> do
        (connectInfo, nodeEnv) <- localNodeConnectInfo nodeConfig
        case com of
          Deploy walletClient opConfig outFile -> do
            walletEnv <- walletClientEnv <$> mgr <*> pure walletClient
            op <- loadOperatorFiles opConfig
            x <- runMonadLogKatip config (Command.runBlockchainAction connectInfo nodeEnv walletEnv (Command.deployRefScripts op))
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
            result <- runMonadLogKatip config (Command.runBlockchainAction connectInfo nodeEnv walletEnv (Command.createPool params))
            case result of
              Left err -> do
                putStrLn (show err)
                exitFailure
              Right k -> traverse_ (\x' -> writeJSONFile x' k) outFile
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

{-| Decode a JSON value from a file
-}
readJSONFile :: FromJSON a => FilePath -> IO (Maybe a)
readJSONFile fp =
  catch (decode <$> BSL.readFile fp) $ \(_ :: SomeException) -> pure Nothing
