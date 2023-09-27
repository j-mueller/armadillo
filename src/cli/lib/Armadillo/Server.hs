{-# LANGUAGE TypeApplications #-}
{-| Server for the HTTP API
-}
module Armadillo.Server(
  runServer
  ) where

import           Armadillo.Api            (API)
import           Armadillo.Cli.Command    (ServerConfig (..))
import           Armadillo.Server.Mock    (mockFarmAPI, mockHistoricApi,
                                           mockLBEAPI, mockLiquidityAPI,
                                           mockTxHistoryAPI)
import           Data.Proxy               (Proxy (..))
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API              (NoContent (..), (:<|>) (..))
import           Servant.Server           (Server, serve)

server :: Server API
server = health :<|> mockHistoricApi :<|> mockLiquidityAPI :<|> mockFarmAPI :<|> mockLBEAPI :<|> mockTxHistoryAPI
  where
    health = pure NoContent

runServer :: ServerConfig -> IO ()
runServer ServerConfig{scPort} =
  let app = serve (Proxy @API) server
  in Warp.run scPort app
