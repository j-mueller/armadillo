{-# LANGUAGE OverloadedStrings #-}
{-| CLI
-}
module Armadillo.Cli(
  runCli
  ) where

import qualified Armadillo.Api              as Api
import           Armadillo.Cli.Command      (Command (..),
                                             RefScriptCommand (..),
                                             ServerConfig (..),
                                             localNodeConnectInfo, parseCommand,
                                             walletClientEnv)
import qualified Armadillo.Command          as Command
import qualified Armadillo.Server           as Server
import           Control.Monad.IO.Class     (MonadIO (..))
import           Convex.MonadLog            (runMonadLogKatip, withKatipLogging)
import           Convex.Wallet.Operator     (loadOperatorFiles)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as LBS
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

mgr :: IO Manager
mgr = newManager defaultManagerSettings

