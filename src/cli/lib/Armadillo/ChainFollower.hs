{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Armadillo.ChainFollower (
  ChainFollowerEnv(..),
  armadilloChainFollower
) where

import qualified Armadillo.ChainFollower.PoolState as PoolState
import           Armadillo.ChainFollower.State     (ChainFollowerState (..),
                                                    startingPoints)
import qualified Armadillo.ChainFollower.State     as State
import           Armadillo.Scripts                 (Scripts (..))
import           Cardano.Api                       (BlockInMode, CardanoMode,
                                                    ChainPoint, Env)
import           Control.Concurrent.STM            (TVar, atomically, writeTVar)
import           Control.Lens                      ((%~), (.~), (^.))
import           Control.Monad                     (guard, when)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Trans.Maybe         (runMaybeT)
import           Convex.MonadLog                   (MonadLog, logInfo, logInfoS,
                                                    runMonadLogKatipT)
import           Convex.NodeClient.ChainTip        (JSONChainPoint (..),
                                                    chainPointText)
import           Convex.NodeClient.Fold            (CatchingUp (..), caughtUp,
                                                    foldClient, getClientPoint,
                                                    resumingFrom, shouldLog)
import           Convex.NodeClient.Resuming        (resumingClient)
import           Convex.NodeClient.Types           (PipelinedLedgerStateClient)
import           Data.Foldable                     (for_)
import           Data.Function                     ((&))
import           Data.Maybe                        (isJust, isNothing)
import qualified Data.Text                         as Text
import qualified Katip                             as K

data ChainFollowerEnv =
  ChainFollowerEnv
    { cfeState     :: TVar ChainFollowerState
    , cfeStateFile :: !FilePath
    , cfeScripts   :: Scripts
    }

armadilloChainFollower :: MonadLog m => K.LogEnv -> K.Namespace -> ChainFollowerEnv -> ChainFollowerState -> [ChainPoint] -> Env -> m PipelinedLedgerStateClient
armadilloChainFollower logEnv ns config iState cliPoints env = do
  logInfoS "Creating chain follower"
  let points' = startingPoints iState ++ cliPoints
  logInfo ("Sync points: " <> Text.intercalate ", " (fmap chainPointText points'))
  pure $ resumingClient points' $ \(resumingFrom -> startingPoint) ->
          foldClient
            (iState{cfsSyncState = Just startingPoint})
            env
            (applyBlock logEnv ns config)

applyBlock :: K.LogEnv -> K.Namespace -> ChainFollowerEnv -> CatchingUp -> ChainFollowerState -> BlockInMode CardanoMode -> IO (Maybe ChainFollowerState)
applyBlock logEnv ns ChainFollowerEnv{cfeState, cfeStateFile, cfeScripts} c oldState block = K.runKatipContextT logEnv () ns $ runMonadLogKatipT $ runMaybeT $ do
  let Scripts{sPoolValidator} = cfeScripts
      (newPoolState, poolEvents) = PoolState.updatePoolState (fst sPoolValidator) block (oldState ^. State.poolState)
  let newFirstBlock = guard (not $ null poolEvents) >> fmap getClientPoint (cfsSyncState oldState)
      newState = updateSyncState c oldState
                    & State.poolState .~ newPoolState
                    & State.firstBlock %~ maybe newFirstBlock Just

  liftIO $ do
    writeState cfeState newState
    when (shouldLog c) (State.writeToFile cfeStateFile newState)

  when (isJust newFirstBlock && isNothing (cfsFirstBlock oldState)) $ for_ newFirstBlock $ \(JSONChainPoint cp) ->
    logInfo $ "First block: " <> chainPointText cp

  when (caughtUp c && maybe False (not . caughtUp) (cfsSyncState oldState)) $ do
    logInfoS "Caught up with node"

  pure newState

writeState :: TVar ChainFollowerState -> ChainFollowerState -> IO ()
writeState tvar state = atomically (writeTVar tvar state)

updateSyncState :: CatchingUp -> ChainFollowerState -> ChainFollowerState
updateSyncState c state = state{cfsSyncState = Just c}
