{-# LANGUAGE TypeApplications #-}
{-| Server for the HTTP API
-}
module Armadillo.Server(
  runServer
  ) where

import           Armadillo.Api            (WIPAPI)
import           Armadillo.Cli.Command    (ServerConfig (..))
import           Data.Proxy               (Proxy (..))
import qualified Network.Wai.Handler.Warp as Warp
import           Servant.API              (NoContent (..))
import           Servant.Server           (Server, serve)

server :: Server WIPAPI
server = health
  where
    health = pure NoContent


runServer :: ServerConfig -> IO ()
runServer ServerConfig{scPort} =
  let app = serve (Proxy @WIPAPI) server
  in Warp.run scPort app
