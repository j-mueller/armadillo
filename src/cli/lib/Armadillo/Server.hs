{-# LANGUAGE TypeApplications #-}
{-| Server for the HTTP API
-}
module Armadillo.Server(
  runServer
  ) where

import           Armadillo.Api                 (FullAPI)
import           Armadillo.ChainFollower.State (ChainFollowerState (..))
import           Armadillo.Cli.Command         (ServerConfig (..))
import           Armadillo.Server.Mock         (mockFarmAPI, mockHistoricApi,
                                                mockInternalAPI, mockLBEAPI,
                                                mockLiquidityAPI,
                                                mockTxHistoryAPI)
import qualified Armadillo.Server.Real         as Real
import           Control.Concurrent.STM        (TVar)
import           Data.Proxy                    (Proxy (..))
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant.API                   (NoContent (..), (:<|>) (..))
import           Servant.Server                (Server, serve)

server :: Maybe (TVar ChainFollowerState) -> Server FullAPI
server tv = (health :<|> maybe mockHistoricApi Real.historicAPI tv :<|> mockLiquidityAPI :<|> mockFarmAPI :<|> mockLBEAPI :<|> mockTxHistoryAPI) :<|> maybe mockInternalAPI Real.internalAPI tv
  where
    health = pure NoContent

runServer :: Maybe (TVar ChainFollowerState) -> ServerConfig -> IO ()
runServer tv ServerConfig{scPort} =
  let app = serve (Proxy @FullAPI) (server tv)
  in Warp.run scPort app
