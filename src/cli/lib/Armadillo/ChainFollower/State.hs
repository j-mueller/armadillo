{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Armadillo.ChainFollower.State(
  ChainFollowerState(..),
  startingPoints,
  writeToFile,
  readFromFile,
  initialState,

  -- ** Lenses
  poolState,
  depositState,
  firstBlock
) where

import           Armadillo.ChainFollower.DepositState (DepositState)
import           Armadillo.ChainFollower.PoolState    (PoolUtxoState)
import qualified Armadillo.Utils                      as Utils
import           Cardano.Api                          (ChainPoint,
                                                       chainTipToChainPoint)
import           Control.Lens                         (makeLensesFor)
import           Convex.NodeClient.ChainTip           (JSONChainPoint (..),
                                                       JSONChainTip (..))
import           Convex.NodeClient.Fold               (CatchingUp (..))
import           Data.Aeson                           (FromJSON, ToJSON)
import           GHC.Generics                         (Generic)

data ChainFollowerState =
  ChainFollowerState
    { cfsSyncState    :: !(Maybe CatchingUp)
    , cfsFirstBlock   :: !(Maybe JSONChainPoint)
    , cfsPoolState    :: !PoolUtxoState
    , cfsDepositState :: !DepositState
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

initialState :: Maybe JSONChainPoint -> ChainFollowerState
initialState cfsFirstBlock = ChainFollowerState{cfsSyncState = Nothing, cfsFirstBlock, cfsPoolState = mempty, cfsDepositState = mempty}

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
readFromFile = fmap (either (const Nothing) Just) . Utils.readJSONFile

makeLensesFor
  [ ("cfsPoolState", "poolState")
  , ("cfsDepositState", "depositState")
  , ("cfsFirstBlock", "firstBlock")
  ] ''ChainFollowerState

