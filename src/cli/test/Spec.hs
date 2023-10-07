{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import           Armadillo.Test.AMMExecutor (loadScriptsConfig)
import qualified Armadillo.Test.Integration as Integration
import qualified Armadillo.Test.UnitTest    as UnitTest
import           Armadillo.Test.Utils       (checkScriptHashes,
                                             scriptsFromScriptsConfig)
import           Test.Tasty                 (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit           (testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "armadillo tests"
  [ Integration.tests
  , UnitTest.tests
  , testCase "hashes" scriptHashTest
  ]

scriptHashTest:: IO ()
scriptHashTest = do
  cfg <- loadScriptsConfig
  s <- scriptsFromScriptsConfig cfg >>= either (error . show) pure
  checkScriptHashes s cfg
