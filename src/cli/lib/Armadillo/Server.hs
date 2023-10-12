{-# LANGUAGE TypeApplications #-}
{-| Server for the HTTP API
-}
module Armadillo.Server(
  runServer,
  TxBuildingContext(..),
  mkTxBuildingContext
  ) where

import           Armadillo.Api                 (FullAPI)
import           Armadillo.ChainFollower.State (ChainFollowerState (..))
import           Armadillo.Cli.Command         (ServerConfig (..))
import           Armadillo.Server.Mock         (mockBuildTxAPI, mockFarmAPI,
                                                mockHistoricApi,
                                                mockInternalAPI, mockLBEAPI,
                                                mockLiquidityAPI,
                                                mockTxHistoryAPI)
import           Armadillo.Server.Real         (TxBuildingContext (..),
                                                mkTxBuildingContext)
import qualified Armadillo.Server.Real         as Real
import           Control.Concurrent.STM        (TVar)
import           Data.Proxy                    (Proxy (..))
import qualified Network.Wai.Handler.Warp      as Warp
import           Servant.API                   (NoContent (..), (:<|>) (..))
import           Servant.Server                (Server, serve)

server :: Maybe (TxBuildingContext, TVar ChainFollowerState) -> Server FullAPI
server tv = (health :<|> maybe mockHistoricApi (Real.historicAPI . snd) tv :<|> mockLiquidityAPI :<|> mockFarmAPI :<|> mockLBEAPI :<|> mockTxHistoryAPI) :<|> maybe mockInternalAPI (Real.internalAPI . snd) tv :<|> maybe mockBuildTxAPI (uncurry Real.buildTxAPI) tv
  where
    health = pure NoContent

runServer :: Maybe (TxBuildingContext, TVar ChainFollowerState) -> ServerConfig -> IO ()
runServer tv ServerConfig{scPort} =
  let app = serve (Proxy @FullAPI) (server tv)
  in Warp.run scPort app
