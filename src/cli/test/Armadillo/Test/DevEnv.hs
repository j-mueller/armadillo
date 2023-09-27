{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Armadillo.Test.DevEnv(
  TestLog(..),
  DevEnv(..),
  CliLog(..),
  withDevEnv
) where

import           Armadillo.Cli.Command      (WalletClientOptions (..))
import           Convex.Devnet.CardanoNode  (NodeLog, RunningNode (..),
                                             withCardanoNodeDevnet)
import           Convex.Devnet.Logging      (Tracer, contramap,
                                             showLogsOnFailure)
import           Convex.Devnet.Utils        (withTempDir)
import           Convex.Devnet.WalletServer (RunningWalletServer (..),
                                             WalletLog, withWallet)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

data TestLog =
  TNodeLog NodeLog
  | TWallet WalletLog
  | TCli CliLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CliLog =
  MsgText{ msgText :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DevEnv =
    DevEnv
      { tracer              :: Tracer IO TestLog
      , tempDir             :: FilePath
      , node                :: RunningNode
      , wallet              :: RunningWalletServer
      , walletClientOptions :: WalletClientOptions
      }

{-| Set up a @DevEnv@ and tear it down after use.
-}
withDevEnv :: (DevEnv -> IO a) -> IO a
withDevEnv action =
  showLogsOnFailure $ \tracer -> do
    withTempDir "armadillo" $ \tempDir -> do
      withCardanoNodeDevnet (contramap TNodeLog tracer) tempDir $ \node ->
        withWallet (contramap TWallet tracer) tempDir node $ \wallet -> do
          action DevEnv{tracer, tempDir, node, wallet, walletClientOptions = walletClientOptions_}

walletClientOptions_ :: WalletClientOptions
walletClientOptions_ =
  WalletClientOptions
    { wcoHost = "localhost"
    , wcoPort = 9988 -- TODO: Magic number defined in Convex.Devnet.WalletServer. Export it there (as part of RunningWalletServer)
    }
