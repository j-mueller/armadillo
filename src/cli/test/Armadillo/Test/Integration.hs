{-# LANGUAGE DeriveAnyClass #-}
module Armadillo.Test.Integration(
  tests
) where

import           Armadillo.Cli.Command       (Command (..),
                                              RefScriptCommand (..),
                                              ServerConfig (..))
import           Armadillo.Test.CliCommand   (CliLog, apiHealth, runCliCommand,
                                              withHttpServer)
import           Armadillo.Test.WalletServer (RunningWalletServer (..),
                                              WalletLog, getUTxOs,
                                              walletClientOptions, withWallet)
import           Convex.Devnet.CardanoNode   (NodeLog, getCardanoNodeVersion,
                                              withCardanoNodeDevnet)
import           Convex.Devnet.Logging       (contramap, showLogsOnFailure)
import           Convex.Devnet.Utils         (withTempDir)
import           Data.Aeson                  (FromJSON, ToJSON)
import           Data.List                   (isInfixOf)
import           GHC.Generics                (Generic)
import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            (assertBool, testCase)

tests :: TestTree
tests = testGroup "integration"
  [ testGroup "setup"
    [ testCase "cardano-node is available" checkCardanoNode
    , testCase "wallet is working" checkWallet
    ]
  , testGroup "HTTP API"
    [ testCase "healthcheck" checkApiHealth
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
        withWallet (contramap TWallet tr) tmp node $ \wllt@RunningWalletServer{rwsOpConfigSigning} -> do
          runCliCommand (contramap TCli tr) tmp (RefScript $ Deploy (walletClientOptions wllt) rwsOpConfigSigning)
          pure ()

data TestLog =
  TNodeLog NodeLog
  | TWallet WalletLog
  | TCli CliLog
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
