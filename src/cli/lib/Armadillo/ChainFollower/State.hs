{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Armadillo.ChainFollower.State(
  ChainFollowerState(..),
  startingPoints,
  writeToFile,
  readFromFile,

  -- ** Lenses
  poolState,
  firstBlock
) where

import           Armadillo.ChainFollower.PoolState (PoolState)
import qualified Armadillo.Utils                   as Utils
import           Cardano.Api                       (ChainPoint,
                                                    chainTipToChainPoint)
import           Control.Lens                      (makeLensesFor)
import           Convex.NodeClient.ChainTip        (JSONChainPoint (..),
                                                    JSONChainTip (..))
import           Convex.NodeClient.Fold            (CatchingUp (..))
import           Data.Aeson                        (FromJSON, ToJSON)
import           GHC.Generics                      (Generic)

data ChainFollowerState =
  ChainFollowerState
    { cfsSyncState  :: !(Maybe CatchingUp)
    , cfsFirstBlock :: !(Maybe JSONChainPoint)
    , cfsPoolState  :: !PoolState
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

{-| The list of 'ChainPoint's that can be used for synchronising with the node
-}
startingPoints :: ChainFollowerState -> [ChainPoint]
startingPoints ChainFollowerState{cfsSyncState, cfsFirstBlock} =
  let syncStateBlock = case cfsSyncState of
        Just CatchingUpWithNode{clientPoint=JSONChainPoint pt} -> [pt]
        Just CaughtUpWithNode{tip=JSONChainTip tp}             -> [chainTipToChainPoint tp]
        _                                                      -> []
      fb = case cfsFirstBlock of
        Just (JSONChainPoint cp) -> [cp]
        _                        -> []
  in mconcat [syncStateBlock, fb]

writeToFile :: FilePath -> ChainFollowerState -> IO ()
writeToFile = Utils.writeJSONFile

readFromFile :: FilePath -> IO (Maybe ChainFollowerState)
readFromFile = Utils.readJSONFile

makeLensesFor
  [ ("cfsPoolState", "poolState")
  , ("cfsFirstBlock", "firstBlock")
  ] ''ChainFollowerState
