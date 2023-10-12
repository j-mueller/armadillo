{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
module Armadillo.Test.DevEnv(
  TestLog(..),
  DevEnv(..),
  withDevEnv,
  createCurrency,
  createPool,
  makeDeposit
) where

import           Armadillo.BuildTx          (PoolOutput)
import           Armadillo.Cli              (readJSONFile)
import           Armadillo.Cli.Command      (Command (..), DebugCommand (..),
                                             Fee (..), PoolCommand (..),
                                             ServerConfig (..),
                                             WalletClientOptions (..))
import           Armadillo.Kupo             (KupoConfig (..))
import           Armadillo.Test.CliCommand  (ChainFollowerStartup (..), CliLog,
                                             RunningHttpServer (..),
                                             deployScripts, mkNodeClientConfig,
                                             runCliCommand, withHttpServer)
import           Armadillo.Test.RunningKupo (KupoLog (..), RunningKupo,
                                             withKupo)
import           Cardano.Api                (AssetId, Quantity (..), TxIn)
import qualified Cardano.Api                as C
import           Convex.Devnet.CardanoNode  (NodeLog, RunningNode (..),
                                             withCardanoNodeDevnet)
import           Convex.Devnet.Logging      (Tracer, contramap,
                                             showLogsOnFailure)
import           Convex.Devnet.Utils        (withTempDir)
import           Convex.Devnet.WalletServer (RunningWalletServer (..),
                                             WalletLog, withWallet)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.String                (IsString (..))
import           GHC.Generics               (Generic)
import           System.IO.Temp             (emptyTempFile)

data TestLog =
  TNodeLog NodeLog
  | TWallet WalletLog
  | TCli CliLog
  | TKupoLog KupoLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DevEnv =
    DevEnv
      { tracer              :: Tracer IO TestLog
      , tempDir             :: FilePath
      , node                :: RunningNode
      , wallet              :: RunningWalletServer
      , httpServer          :: RunningHttpServer
      , walletClientOptions :: WalletClientOptions
      , kupo                :: RunningKupo
      }

{-| Set up a @DevEnv@ and tear it down after use.
-}
withDevEnv :: (DevEnv -> IO a) -> IO a
withDevEnv action =
  showLogsOnFailure $ \tracer -> do
    withTempDir "armadillo" $ \tempDir -> do
      withCardanoNodeDevnet (contramap TNodeLog tracer) tempDir $ \node ->
        withWallet (contramap TWallet tracer) tempDir node $ \wallet -> do
          _refScripts <- deployScripts (contramap TCli tracer) tempDir node walletClientOptions_ (rwsOpConfigSigning wallet)
          let kupoCfg = KupoConfig "localhost" 9999
          withKupo (contramap TKupoLog tracer) tempDir node kupoCfg $ \kupo ->
            withHttpServer (contramap TCli tracer) tempDir ServerConfig{scPort = 9088} (StartChainFollower tempDir node kupoCfg) $ \httpServer ->
              action DevEnv{tracer, tempDir, node, wallet, walletClientOptions = walletClientOptions_, httpServer, kupo}

walletClientOptions_ :: WalletClientOptions
walletClientOptions_ =
  WalletClientOptions
    { wcoHost = "localhost"
    , wcoPort = 9988 -- TODO: Magic number defined in Convex.Devnet.WalletServer. Export it there (as part of RunningWalletServer)
    }

createCurrency :: DevEnv -> String -> IO AssetId
createCurrency DevEnv{tempDir, wallet=RunningWalletServer{rwsOpConfigSigning}, node, tracer, walletClientOptions} name = do
  outFile <- emptyTempFile tempDir "script-hash"
  runCliCommand (contramap TCli tracer) tempDir (Debug (mkNodeClientConfig node) (Just outFile) $ CreateCurrency walletClientOptions rwsOpConfigSigning name)
  scriptHash <- readJSONFile outFile >>= either (error . (<>) ("Unable to read JSON file " <> outFile <> ": ")) pure
  pure $ C.AssetId (C.PolicyId scriptHash) (fromString name)

createPool :: DevEnv -> Fee -> AssetId -> AssetId -> IO (PoolOutput TxIn)
createPool DevEnv{tempDir, wallet=RunningWalletServer{rwsOpConfigSigning}, node, tracer, walletClientOptions} fee assetX assetY = do
  outFile <- emptyTempFile tempDir "active-pool"
  runCliCommand (contramap TCli tracer) tempDir (Pool (mkNodeClientConfig node) (Just outFile) $ Create walletClientOptions rwsOpConfigSigning fee (assetX, 50) (assetY, 50))
  readJSONFile outFile >>= either (error . (<>) ("Unable to read JSON file " <> outFile <> ": ")) pure

{-| Make a deposit to a pool
-}
makeDeposit :: DevEnv -> AssetId -> AssetId -> Quantity -> IO ()
makeDeposit DevEnv{httpServer, tempDir, wallet=RunningWalletServer{rwsOpConfigSigning}, node, tracer, walletClientOptions} assetX assetY q = do
  outFile <- emptyTempFile tempDir "make-deposit"
  runCliCommand (contramap TCli tracer) tempDir (Pool (mkNodeClientConfig node) (Just outFile) $ Deposit walletClientOptions rwsOpConfigSigning (rhApiOptions httpServer) assetX assetY q)
