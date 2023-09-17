module Armadillo.Cli.Command(
  CliOptions(..),
  Command(..),
  ServerConfig(..),
  parseCommand
) where

import           Options.Applicative (CommandFields, Mod, Parser, auto, command,
                                      fullDesc, help, info, long, option,
                                      progDesc, strOption, subparser)

data CliOptions =
  CliOptions
    { cliNodeSocket :: FilePath
    , cliNodeConfig :: FilePath
    }
    deriving stock (Eq, Ord, Show)

data Command =
  StartServer{serverConfig :: ServerConfig } -- ^ Serve the API
  | WriteAPIFile{filePath :: FilePath } -- ^ Write the API to a file

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
      ]

startServer :: Mod CommandFields Command
startServer = command "start-server" $
  info (StartServer <$> parseServerConfig) (fullDesc <> progDesc "Start the server")

writeApi :: Mod CommandFields Command
writeApi = command "write-api" $
  info (WriteAPIFile <$> parseAPIFile) (fullDesc <> progDesc "Write the OpenAPI3 specification of the API to a JSON file")

parseServerConfig :: Parser ServerConfig
parseServerConfig =
  ServerConfig
    <$> option auto (long "http.port" <> help "Port for the HTTP server")

parseAPIFile :: Parser FilePath
parseAPIFile =
  strOption (long "api.file" <> help "The JSON file where the API documentation should be written")
