{-# LANGUAGE DeriveAnyClass #-}
module Armadillo.Test.Integration(
  tests,
  runDevEnv
) where

import           Armadillo.Cli.Command      (Command (..),
                                             NodeClientConfig (..),
                                             RefScriptCommand (..),
                                             ServerConfig (..),
                                             WalletClientOptions (..))
import           Armadillo.Test.CliCommand  (CliLog, apiHealth, apiPairs,
                                             apiTransactions, runCliCommand,
                                             withHttpServer)
import           Armadillo.Test.Utils       (checkRefScripts)
import           Convex.Devnet.CardanoNode  (NodeLog, RunningNode (..),
                                             getCardanoNodeVersion,
                                             withCardanoNodeDevnet)
import           Convex.Devnet.Logging      (contramap, showLogsOnFailure)
import           Convex.Devnet.Utils        (withTempDir)
import           Convex.Devnet.WalletServer (RunningWalletServer (..),
                                             WalletLog, getUTxOs, withWallet)
import           Data.Aeson                 (FromJSON, ToJSON)
import           Data.List                  (isInfixOf)
import           GHC.Generics               (Generic)
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
    , testCase "deployScripts" checkDeployScript
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
        apiTransactions server >>= assertBool "there should be more than 0 transactions" . (not . null)


checkWallet :: IO ()
checkWallet =
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withCardanoNodeDevnet (contramap TNodeLog tr) tmp $ \node ->
        withWallet (contramap TWallet tr) tmp node $ \wllt -> do
          _x <- getUTxOs wllt
          pure ()

checkDeployScript :: IO ()
checkDeployScript =
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withCardanoNodeDevnet (contramap TNodeLog tr) tmp $ \node ->
        withWallet (contramap TWallet tr) tmp node $ \RunningWalletServer{rwsOpConfigSigning} -> do
          let outFile = tmp </> "reference-scripts.json"
          runCliCommand (contramap TCli tr) tmp (RefScript (nodeClientConfig node) $ Deploy walletClientOptions rwsOpConfigSigning outFile)
          checkRefScripts node outFile

data TestLog =
  TNodeLog NodeLog
  | TWallet WalletLog
  | TCli CliLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

nodeClientConfig :: RunningNode -> NodeClientConfig
nodeClientConfig RunningNode{rnNodeConfigFile, rnNodeSocket} =
  NodeClientConfig
    { nccCardanoNodeSocket = rnNodeSocket
    , nccCardanoNodeConfigFile = rnNodeConfigFile
    }

walletClientOptions :: WalletClientOptions
walletClientOptions =
  WalletClientOptions
    { wcoHost = "localhost"
    , wcoPort = 9988 -- TODO: Magic number defined in Convex.Devnet.WalletServer. Export it there (as part of RunningWalletServer)
    }

{-| Start a dev env and wait for input on stdin before shutting it down.
-}
runDevEnv :: IO ()
runDevEnv =
  showLogsOnFailure $ \tr -> do
    withTempDir "armadillo" $ \tmp -> do
      withCardanoNodeDevnet (contramap TNodeLog tr) tmp $ \node ->
        withWallet (contramap TWallet tr) tmp node $ \RunningWalletServer{rwsOpConfigSigning} -> do
          withHttpServer (contramap TCli tr) tmp ServerConfig{scPort = 9088} $ \_server -> do
            let outFile = tmp </> "reference-scripts.json"
            putStrLn "Devnet running. Wallet port: 9988. API port: 9088"
            runCliCommand (contramap TCli tr) tmp (RefScript (nodeClientConfig node) $ Deploy walletClientOptions rwsOpConfigSigning outFile)
            putStrLn $ "Reference scripts have been deployed to " <> outFile
            putStrLn "Press any key to exit"
            readLn
