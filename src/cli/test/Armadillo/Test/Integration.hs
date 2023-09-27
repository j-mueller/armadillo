{-# LANGUAGE DeriveAnyClass #-}
module Armadillo.Test.Integration(
  tests,
  runDevEnv
) where

import qualified Armadillo.Api              as Api
import           Armadillo.Cli.Command      (Command (..), Fee (..),
                                             RefScriptCommand (..),
                                             ServerConfig (..))
import qualified Armadillo.Server.Mock      as Mock
import           Armadillo.Test.CliCommand  (apiHealth, apiPairs,
                                             apiTransactions, createCurrency,
                                             createPool, runCliCommand,
                                             withHttpServer)
import           Armadillo.Test.DevEnv      (DevEnv (..), TestLog (..),
                                             withDevEnv)
import           Armadillo.Test.Utils       (availableTokens, checkRefScripts,
                                             nodeClientConfig)
import qualified Cardano.Api                as C
import           Convex.Devnet.CardanoNode  (getCardanoNodeVersion,
                                             withCardanoNodeDevnet)
import           Convex.Devnet.Logging      (contramap, showLogsOnFailure)
import           Convex.Devnet.Utils        (withTempDir)
import           Convex.Devnet.WalletServer (RunningWalletServer (..), getUTxOs,
                                             withWallet)
import           Data.List                  (isInfixOf)
import           System.FilePath            ((</>))
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, assertEqual, testCase)

tests :: TestTree
tests = testGroup "integration"
  [ testGroup "setup"
    [ testCase "cardano-node is available" checkCardanoNode
    , testCase "wallet is working" checkWallet
    ]
  , testGroup "HTTP API"
    [ testCase "healthcheck" checkApiHealth
    , testCase "mock API" checkMockAPI
    ]
  , testGroup "Commands"
    [ testCase "deployScripts" checkDeployScript
    , testCase "create currency" checkCreateCurrency
    , testCase "create pool" checkCreatePool
    ]
  ]

checkCardanoNode :: IO ()
checkCardanoNode = do
  let expectedVersion = "8.1.1"
  getCardanoNodeVersion >>= assertBool ("cardano-node version should be " <> expectedVersion) . isInfixOf expectedVersion

checkApiHealth :: IO ()
checkApiHealth = do
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withHttpServer tr tmp ServerConfig{scPort = 9088} $ \server -> do
        apiHealth server

checkMockAPI :: IO ()
checkMockAPI = do
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withHttpServer tr tmp ServerConfig{scPort = 9088} $ \server -> do
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
checkDeployScript = withDevEnv $ \DevEnv{tracer, tempDir, node, wallet=RunningWalletServer{rwsOpConfigSigning}, walletClientOptions} -> do
  let outFile = tempDir </> "reference-scripts.json"
  runCliCommand (contramap TCli tracer) tempDir (RefScript (nodeClientConfig node) $ Deploy walletClientOptions rwsOpConfigSigning outFile)
  checkRefScripts node outFile

checkCreateCurrency :: IO ()
checkCreateCurrency = withDevEnv $ \de@DevEnv{wallet, tracer} -> do
  assetId <- createCurrency de "token1"
  q <- availableTokens (contramap TWallet tracer) wallet assetId
  assertEqual "Should have 1000 tokens" 1000 q

checkCreatePool :: IO ()
checkCreatePool = withDevEnv $ \de -> do
  asset1 <- createCurrency de "token1"
  _p <- createPool de (Fee 123) asset1 C.AdaAssetId
  pure ()


{-| Start a dev env and wait for input on stdin before shutting it down.
-}
runDevEnv :: IO ()
runDevEnv = withDevEnv $ \DevEnv{tracer, tempDir, node, wallet=RunningWalletServer{rwsOpConfigSigning}, walletClientOptions} -> do
  let outFile = tempDir </> "reference-scripts.json"
  putStrLn "Devnet running. Wallet port: 9988. API port: 9088"
  runCliCommand (contramap TCli tracer) tempDir (RefScript (nodeClientConfig node) $ Deploy walletClientOptions rwsOpConfigSigning outFile)
  putStrLn $ "Reference scripts have been deployed to " <> outFile
  putStrLn "Press any key to exit"
  readLn
