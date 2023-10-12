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
  createCurrency,
  createPool,
  makeDeposit
) where

import           Armadillo.Api                      (CreatePoolArgs (..),
                                                     WrappedTx (..))
import qualified Armadillo.Api                      as Api
import           Armadillo.BuildTx                  (PoolOutput)
import           Armadillo.Cli                      (readJSONFile)
import           Armadillo.Cli.Command              (Command (..),
                                                     DebugCommand (..),
                                                     Fee (..), PoolCommand (..),
                                                     ServerConfig (..),
                                                     WalletClientOptions (..))
import           Armadillo.Kupo                     (KupoConfig (..))
import           Armadillo.Test.CliCommand          (ChainFollowerStartup (..),
                                                     CliLog,
                                                     RunningHttpServer (..),
                                                     deployScripts,
                                                     mkNodeClientConfig,
                                                     runCliCommand,
                                                     withHttpServer)
import           Armadillo.Test.RunningKupo         (KupoLog (..), RunningKupo,
                                                     withKupo)
import           Cardano.Api                        (AssetId, Quantity (..),
                                                     TxIn)
import qualified Cardano.Api                        as C
import           Convex.Class                       (runMonadBlockchainCardanoNodeT,
                                                     sendTx)
import           Convex.Devnet.CardanoNode          (NodeLog, RunningNode (..),
                                                     withCardanoNodeDevnet)
import           Convex.Devnet.Logging              (Tracer, contramap,
                                                     showLogsOnFailure)
import           Convex.Devnet.Utils                (failure, withTempDir)
import           Convex.Devnet.WalletServer         (RunningWalletServer (..),
                                                     WalletLog, withWallet)
import           Convex.MonadLog                    (runMonadLogIgnoreT)
import           Convex.NodeClient.WaitForTxnClient (runMonadBlockchainWaitingT)
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
makeDeposit :: DevEnv -> AssetId -> AssetId -> Quantity -> IO ()
makeDeposit DevEnv{httpServer, tempDir, wallet=RunningWalletServer{rwsOpConfigSigning}, node, tracer, walletClientOptions} assetX assetY q = do
  outFile <- emptyTempFile tempDir "make-deposit"
  runCliCommand (contramap TCli tracer) tempDir (Pool (mkNodeClientConfig node) (Just outFile) $ Deposit walletClientOptions rwsOpConfigSigning (rhApiOptions httpServer) assetX assetY q)

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
