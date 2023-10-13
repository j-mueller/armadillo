{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Armadillo.Test.DevEnv(
  TestLog(..),
  DevEnv(..),
  withDevEnv,
  dumpUtxoSet,
  createCurrency,
  createPool,
  makeDeposit,
  makeSwap,
  waitForKupoSync
) where

import           Armadillo.Api                      (CreatePoolArgs (..),
                                                     MakeDepositArgs (..),
                                                     SwapArgs (..),
                                                     WrappedTx (..))
import qualified Armadillo.Api                      as Api
import           Armadillo.BuildTx                  (DepositOutput, PoolOutput,
                                                     SwapOutput)
import           Armadillo.Cli                      (readJSONFile)
import           Armadillo.Cli.Command              (Command (..),
                                                     DebugCommand (..),
                                                     Fee (..),
                                                     ServerConfig (..))
import           Armadillo.Kupo                     (KupoConfig (..),
                                                     KupoHealth (..))
import qualified Armadillo.Kupo                     as Kupo
import           Armadillo.Test.CliCommand          (ChainFollowerStartup (..),
                                                     CliLog,
                                                     RunningHttpServer (..),
                                                     deployScripts,
                                                     mkNodeClientConfig,
                                                     runCliCommand,
                                                     withHttpServer)
import           Armadillo.Test.RunningKupo         (KupoLog (..),
                                                     RunningKupo (..), withKupo)
import           Cardano.Api                        (AssetId, Quantity (..),
                                                     SlotNo (..), TxIn)
import qualified Cardano.Api                        as C
import           Control.Concurrent                 (threadDelay)
import           Control.Monad                      (unless)
import           Convex.Class                       (runMonadBlockchainCardanoNodeT,
                                                     sendTx)
import           Convex.Devnet.CardanoNode          (NodeLog, RunningNode (..),
                                                     withCardanoNodeDevnet)
import           Convex.Devnet.Logging              (Tracer, contramap,
                                                     showLogsOnFailure,
                                                     traceWith)
import qualified Convex.Devnet.NodeQueries          as Queries
import           Convex.Devnet.Utils                (failure, withTempDir)
import           Convex.Devnet.WalletServer         (RunningWalletServer (..),
                                                     WalletLog, withWallet)
import           Convex.MonadLog                    (runMonadLogIgnoreT)
import           Convex.NodeClient.WaitForTxnClient (runMonadBlockchainWaitingT)
import qualified Convex.NodeQueries                 as NodeQueries
import           Convex.Wallet.Operator             (Operator, Signing,
                                                     operatorWalletID,
                                                     signTxOperator)
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.String                        (IsString (..))
import           GHC.Generics                       (Generic)
import           Servant.Client                     (ClientEnv, ClientError)
import           System.IO.Temp                     (emptyTempFile)

data TestLog =
  TNodeLog NodeLog
  | TWallet WalletLog
  | TCli CliLog
  | TKupoLog KupoLog
  | TWaitingForKupoSync{ nodeSlot :: SlotNo, kupoSlot :: SlotNo }
  | TDumpUtxoSet (C.UTxO C.BabbageEra)
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DevEnv =
    DevEnv
      { tracer     :: Tracer IO TestLog
      , tempDir    :: FilePath
      , node       :: RunningNode
      , wallet     :: RunningWalletServer
      , httpServer :: RunningHttpServer
      , kupo       :: RunningKupo
      }

{-| Set up a @DevEnv@ and tear it down after use.
-}
withDevEnv :: (DevEnv -> IO a) -> IO a
withDevEnv action =
  showLogsOnFailure $ \tracer -> do
    withTempDir "armadillo" $ \tempDir -> do
      withCardanoNodeDevnet (contramap TNodeLog tracer) tempDir $ \node ->
        withWallet (contramap TWallet tracer) tempDir node $ \wallet -> do
          let kupoCfg = KupoConfig "localhost" 9999
          withKupo (contramap TKupoLog tracer) tempDir node kupoCfg $ \kupo -> do
              withHttpServer (contramap TCli tracer) tempDir ServerConfig{scPort = 9088} (StartChainFollower tempDir node kupoCfg) $ \httpServer -> do
                _refScripts <- deployScripts (contramap TCli tracer) tempDir node kupoCfg (rwsOpConfigSigning wallet)
                let de = DevEnv{tracer, tempDir, node, wallet, httpServer, kupo}
                waitForKupoSync de
                action de

{-| Write the UTxO set to the log
-}
dumpUtxoSet :: DevEnv -> IO ()
dumpUtxoSet DevEnv{tracer, node=RunningNode{rnNetworkId, rnNodeSocket}} = do
  Queries.queryUTxOWhole rnNetworkId rnNodeSocket >>= traceWith tracer . TDumpUtxoSet

createCurrency :: DevEnv -> String -> IO AssetId
createCurrency DevEnv{tempDir, wallet=RunningWalletServer{rwsOpConfigSigning}, node, tracer, kupo=RunningKupo{rkConfig}} name = do
  outFile <- emptyTempFile tempDir "script-hash"
  runCliCommand (contramap TCli tracer) tempDir (Debug (mkNodeClientConfig node) (Just outFile) $ CreateCurrency rkConfig rwsOpConfigSigning name)
  scriptHash <- readJSONFile outFile >>= either (error . (<>) ("Unable to read JSON file " <> outFile <> ": ")) pure
  pure $ C.AssetId (C.PolicyId scriptHash) (fromString name)

createPool :: DevEnv -> Fee -> (AssetId, Quantity) -> (AssetId, Quantity) -> IO (PoolOutput TxIn)
createPool devEnv@DevEnv{wallet=RunningWalletServer{rwsOperator}, node} (Fee fee) (assetX, Quantity cpoQuantityX) (assetY, Quantity cpoQuantityY) = do
  let (cpoPublicKeyHash, cpoStakingCredential) = operatorWalletID rwsOperator
      cpoAssetX = Api.fromCardanoAssetId assetX
      cpoAssetY = Api.fromCardanoAssetId assetY
      args = CreatePoolArgs{cpoAssetX, cpoAssetY, cpoQuantityX, cpoQuantityY, cpoFeeNumerator = fee, cpoPublicKeyHash, cpoStakingCredential}
  (WrappedTx k, poolOut) <- runAPIRequest (Api.buildCreatePoolTx args) devEnv
  _ <- signAndSubmitTx node rwsOperator k
  pure poolOut

{-| Make a deposit to a pool
-}
makeDeposit :: DevEnv -> AssetId -> AssetId -> (Quantity, Quantity) -> IO (DepositOutput TxIn)
makeDeposit devEnv@DevEnv{wallet=RunningWalletServer{rwsOperator}, node} assetX assetY (Quantity mdQuantityX, Quantity mdQuantityY) = do
  let (mdPublicKeyHash, mdStakingCredential) = operatorWalletID rwsOperator
      args =
        MakeDepositArgs
          { mdAssetX = Api.fromCardanoAssetId assetX
          , mdAssetY = Api.fromCardanoAssetId assetY
          , mdQuantityX
          , mdQuantityY
          , mdPublicKeyHash
          , mdStakingCredential
          }
  (WrappedTx k, poolOut) <- runAPIRequest (Api.buildMakeDepositTx args) devEnv
  _ <- signAndSubmitTx node rwsOperator k
  pure poolOut

{-| Make a swap to a pool
-}
makeSwap :: DevEnv -> AssetId -> AssetId -> Quantity -> IO (SwapOutput TxIn)
makeSwap devEnv@DevEnv{wallet=RunningWalletServer{rwsOperator}, node} assetX assetY (Quantity saBaseAmount) = do
  let (saRewardPkh, saStakePkh) = operatorWalletID rwsOperator
      args =
        SwapArgs
        { saBaseToken = Api.fromCardanoAssetId assetX
        , saQuoteToken = Api.fromCardanoAssetId assetY
        , saExFeePerTokenNum = 1
        , saExFeePerTokenDen = 100
        , saRewardPkh
        , saStakePkh
        , saBaseAmount
        , saMinQuoteAmount = 1
        }
  (WrappedTx k, swapOut) <- runAPIRequest (Api.buildSwapTx args) devEnv
  _ <- signAndSubmitTx node rwsOperator k
  pure swapOut

{-| Call the chain follower API, failing on errors
-}
runAPIRequest :: (ClientEnv -> IO (Either ClientError a)) -> DevEnv -> IO a
runAPIRequest request DevEnv{httpServer=RunningHttpServer{rhClient}} =
  request rhClient >>= \case
    Left err -> failure ("runAPIRequest: Failed with " <> show err)
    Right x  -> pure x

{-| Sign and submit a balanced transaction
-}
signAndSubmitTx :: RunningNode -> Operator Signing -> C.Tx C.BabbageEra -> IO C.TxId
signAndSubmitTx RunningNode{rnConnectInfo=(connectInfo, env)} operator t = do
  x <- runMonadLogIgnoreT $ runMonadBlockchainCardanoNodeT @() connectInfo $
        runMonadBlockchainWaitingT connectInfo env $ do
          sendTx (signTxOperator operator t)
  either (fail . (<>) "signAndSubmitTx: " . show @_) pure x

waitForKupoSync :: DevEnv -> IO ()
waitForKupoSync DevEnv{kupo=RunningKupo{rkClientEnv}, node=RunningNode{rnConnectInfo=(connectInfo, _env)}, tracer} = do
  NodeQueries.queryTip connectInfo >>= \case
    C.ChainPointAtGenesis -> fail "Unexpected genesis"
    C.ChainPoint nodeSlot _ -> go nodeSlot

  where
    go nodeSlot = do
      KupoHealth{most_recent_node_tip} <- Kupo.getKupoHealth rkClientEnv >>= either (fail . show) pure
      let kupoSlot = C.SlotNo $ fromInteger most_recent_node_tip
      unless (kupoSlot >= nodeSlot) $ do
        traceWith tracer $ TWaitingForKupoSync{nodeSlot, kupoSlot}
        threadDelay 1_000_000
        go nodeSlot

