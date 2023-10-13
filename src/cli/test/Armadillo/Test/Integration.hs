{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Armadillo.Test.Integration(
  tests,
  runDevEnv
) where

import qualified Armadillo.Api              as Api
import           Armadillo.BuildTx          (PoolOutput (..))
import           Armadillo.Cli.Command      (Command (..), Fee (..),
                                             RefScriptCommand (..),
                                             ServerConfig (..))
import qualified Armadillo.Server.Mock      as Mock
import           Armadillo.Test.CliCommand  (ChainFollowerStartup (..),
                                             apiHealth, apiPairs, apiPools,
                                             apiTransactions,
                                             mkNodeClientConfig, runCliCommand,
                                             withHttpServer)
import           Armadillo.Test.DevEnv      (DevEnv (..), TestLog (..),
                                             createCurrency, createPool,
                                             makeDeposit, waitForKupoSync,
                                             withDevEnv)
import           Armadillo.Test.RunningKupo (RunningKupo (..))
import           Armadillo.Test.Utils       (availableTokens, checkRefScripts)
import qualified Cardano.Api                as C
import           Control.Concurrent         (threadDelay)
import           Convex.Devnet.CardanoNode  (getCardanoNodeVersion,
                                             withCardanoNodeDevnet)
import           Convex.Devnet.Logging      (contramap, showLogsOnFailure)
import           Convex.Devnet.Utils        (withTempDir)
import           Convex.Devnet.WalletServer (RunningWalletServer (..), getUTxOs,
                                             withWallet)
import           Data.List                  (isInfixOf)
import           System.FilePath            ((</>))
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, assertEqual, testCase,
                                             testCaseSteps)

tests :: TestTree
tests = testGroup "integration"
  [ testGroup "setup"
    [ testCase "T1: cardano-node is available" checkCardanoNode
    , testCase "T2: wallet is working" checkWallet
    ]
  , testGroup "HTTP"
    [ testCase "T3: healthcheck" checkApiHealth
    , testCase "T4: mock API" checkMockAPI
    ]
  , testCaseSteps "end-to-end" endToEndTest
  ]

checkCardanoNode :: IO ()
checkCardanoNode = do
  let expectedVersion = "8.1.1"
  getCardanoNodeVersion >>= assertBool ("cardano-node version should be " <> expectedVersion) . isInfixOf expectedVersion

checkApiHealth :: IO ()
checkApiHealth = do
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withHttpServer tr tmp ServerConfig{scPort = 9088} DontStartChainFollower $ \server -> do
        apiHealth server

checkMockAPI :: IO ()
checkMockAPI = do
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withHttpServer tr tmp ServerConfig{scPort = 9088} DontStartChainFollower $ \server -> do
        apiPairs server >>= assertEqual "there should be two pairs" 2 . length
        let p = Mock.djedAdaPair
        apiTransactions server (Api.pairID p) >>= assertBool "there should be more than 0 transactions" . (not . null)

checkWallet :: IO ()
checkWallet =
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withCardanoNodeDevnet (contramap TNodeLog tr) tmp $ \node ->
        withWallet (contramap TWallet tr) tmp node $ \wllt -> do
          _x <- getUTxOs wllt
          pure ()

checkDeployScript :: IO ()
checkDeployScript = withDevEnv $ \DevEnv{tracer, tempDir, node, wallet=RunningWalletServer{rwsOpConfigSigning}, kupo=RunningKupo{rkConfig}} -> do
  let outFile = tempDir </> "reference-scripts.json"
  runCliCommand (contramap TCli tracer) tempDir (RefScript (mkNodeClientConfig node) $ Deploy rkConfig rwsOpConfigSigning outFile)
  checkRefScripts node outFile

checkCreateCurrency :: IO ()
checkCreateCurrency = withDevEnv $ \de@DevEnv{wallet, tracer} -> do
  assetId <- createCurrency de "token1"
  q <- availableTokens (contramap TWallet tracer) wallet assetId
  assertEqual "Should have 1000 tokens" 1000 q

endToEndTest :: (String -> IO ()) -> IO ()
endToEndTest step = withDevEnv $ \de@DevEnv{httpServer, tracer, wallet} -> do
  threadDelay 3_000_000
  step "Creating currency"
  asset1 <- createCurrency de "token1"
  threadDelay 3_000_000
  availableTokens (contramap TWallet tracer) wallet asset1 >>= assertEqual "Should have 1000 tokens" 1000
  step "Creating pool"
  waitForKupoSync de
  _p <- createPool de (Fee 123) (asset1, 100) (C.AdaAssetId, 100)
  threadDelay 3_000_000
  waitForKupoSync de
  apiPairs httpServer >>= assertEqual "there should be one pair" 1 . length
  waitForKupoSync de
  step "Querying pools"
  [PoolOutput{poTxIn=oldTxI}] <- apiPools httpServer
  step "Making a deposit"
  threadDelay 3_000_000
  waitForKupoSync de
  _ <- makeDeposit de asset1 C.AdaAssetId (50, 50)
  -- threadDelay 10_000_000
  pure ()
  -- step "Checking that the deposit has been applied"
  -- [PoolOutput{poTxIn=newTxI}] <- apiPools httpServer
  -- assertBool "Old should be different from new" (oldTxI /= newTxI)

{-| Start a dev env and wait for input on stdin before shutting it down.
-}
runDevEnv :: IO ()
runDevEnv = withDevEnv $ \DevEnv{tracer, tempDir, node, wallet=RunningWalletServer{rwsOpConfigSigning}, kupo=RunningKupo{rkConfig}} -> do
  let outFile = tempDir </> "reference-scripts.json"
  putStrLn "Devnet running. Wallet port: 9988. API port: 9088"
  runCliCommand (contramap TCli tracer) tempDir (RefScript (mkNodeClientConfig node) $ Deploy rkConfig rwsOpConfigSigning outFile)
  putStrLn $ "Reference scripts have been deployed to " <> outFile
  putStrLn "Press any key to exit"
  readLn
