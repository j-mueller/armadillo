{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Armadillo.Test.Utils(
  checkRefScripts,
  availableTokens,


  -- * Options / config
  mkNodeClientConfig
) where

import           Armadillo.BuildTx          (ReferenceScripts (..))
import           Armadillo.Cli.Command      (NodeClientConfig (..))
import           Cardano.Api                (AssetId, Quantity, TxIn)
import qualified Cardano.Api                as C
import qualified Cardano.Api.Shelley        as C
import qualified Control.Lens               as L
import           Convex.Devnet.CardanoNode  (RunningNode (..))
import           Convex.Devnet.Logging      (Tracer, traceWith)
import           Convex.Devnet.NodeQueries  (queryUTxOWhole)
import           Convex.Devnet.WalletServer (RunningWalletServer,
                                             WalletLog (..), getUTxOs)
import qualified Convex.Lenses              as L
import           Convex.Utxos               (totalBalance)
import           Data.Aeson                 (decode)
import qualified Data.ByteString.Lazy       as BSL
import           Data.Foldable              (traverse_)
import qualified Data.Map                   as Map

checkRefScripts :: RunningNode -> FilePath -> IO ()
checkRefScripts RunningNode{rnNodeSocket, rnNetworkId} fp = do
  ReferenceScripts{refScriptPool, refScriptSwap, refScriptDeposit, refScriptRedeem} :: ReferenceScripts TxIn  <- decode <$> BSL.readFile fp >>= maybe (error $ "checkRefScripts: failed to load file " <> fp) pure
  C.UTxO utxo <- queryUTxOWhole rnNetworkId rnNodeSocket
  let check :: String -> TxIn -> IO ()
      check name txI =
        case Map.lookup txI utxo of
          Nothing -> error (name <> ": Failed to find reference script")
          Just (L.view (L._TxOut . L._4) -> C.ReferenceScript _ (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) _))  -> pure ()
          Just txOut -> error (name <> ": Unexpected output: " <> show txOut)
  traverse_ (uncurry check) [("refScriptPool", refScriptPool), ("refScriptSwap", refScriptSwap), ("refScriptDeposit", refScriptDeposit), ("refScriptRedeem", refScriptRedeem)]

{-| Return the amount of tokens of the @AssetId@ that the wallet has
-}
availableTokens :: Tracer IO WalletLog -> RunningWalletServer -> AssetId -> IO Quantity
availableTokens tracer rws assetId = do
  totalBal <- totalBalance <$> getUTxOs rws
  traceWith tracer (WMsgText $ "Total balance: " <> C.renderValuePretty totalBal)
  let result = C.selectAsset totalBal assetId
  traceWith tracer (WMsgText $ "Asset ID: " <> C.renderValuePretty (C.valueFromList [(assetId, 1000)]))
  pure result

mkNodeClientConfig :: RunningNode -> NodeClientConfig
mkNodeClientConfig RunningNode{rnNodeConfigFile, rnNodeSocket} =
  NodeClientConfig
    { nccCardanoNodeSocket = rnNodeSocket
    , nccCardanoNodeConfigFile = rnNodeConfigFile
    }
