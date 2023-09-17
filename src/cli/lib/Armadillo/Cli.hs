{-| CLI
-}
module Armadillo.Cli(
  runCli
  ) where

import qualified Armadillo.Api          as Api
import           Armadillo.Cli.Command  (Command (..), ServerConfig (..),
                                         parseCommand)
import qualified Armadillo.Server       as Server
import           Control.Monad.IO.Class (MonadIO (..))
import           Options.Applicative    (customExecParser, disambiguate, helper,
                                         idm, info, prefs, showHelpOnEmpty,
                                         showHelpOnError)

runCli :: IO ()
runCli = do
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
