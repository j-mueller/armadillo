module Armadillo.Test.Integration(
  tests
) where

import           Armadillo.Cli.Command     (ServerConfig (..))
import           Armadillo.Test.CliCommand (apiHealth, withHttpServer)
import           Convex.Devnet.CardanoNode (getCardanoNodeVersion)
import           Convex.Devnet.Logging     (showLogsOnFailure)
import           Convex.Devnet.Utils       (failure, withTempDir)
import           Data.List                 (isInfixOf)
import           Test.Tasty                (TestTree, testGroup)
import           Test.Tasty.HUnit          (Assertion, assertBool, assertEqual,
                                            testCase, testCaseSteps)

tests :: TestTree
tests = testGroup "integration"
  [ testGroup "setup"
    [ testCase "cardano-node is available" checkCardanoNode
    ]
  , testGroup "HTTP API"
    [ testCase "healthcheck" checkApiHealth

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
