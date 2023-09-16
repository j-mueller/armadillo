{-# LANGUAGE GADTs        #-}
{-# LANGUAGE ViewPatterns #-}
module Armadillo.Test.Utils(
  checkRefScripts
) where

import           Armadillo.BuildTx         (ReferenceScripts (..))
import           Cardano.Api               (TxIn)
import qualified Cardano.Api               as C
import qualified Cardano.Api.Shelley       as C
import qualified Control.Lens              as L
import           Convex.Devnet.CardanoNode (RunningNode (..))
import           Convex.Devnet.NodeQueries (queryUTxOWhole)
import qualified Convex.Lenses             as L
import           Data.Aeson                (decode)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Foldable             (traverse_)
import qualified Data.Map                  as Map

checkRefScripts :: RunningNode -> FilePath -> IO ()
checkRefScripts RunningNode{rnNodeSocket, rnNetworkId} fp = do
  ReferenceScripts{refScriptPool, refScriptSwap, refScriptDeposit, refScriptRedeem} :: ReferenceScripts TxIn  <- decode <$> BSL.readFile fp >>= maybe (error $ "checkRefScripts: failed to load file " <> fp) pure
  C.UTxO utxo <- queryUTxOWhole rnNetworkId rnNodeSocket
  let check :: String -> TxIn -> IO ()
      check name txI =
        case Map.lookup txI utxo of
          Nothing -> error (name <> ": Failed to find refScriptPool")
          Just (L.view (L._TxOut . L._4) -> C.ReferenceScript _ (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) _))  -> pure ()
          Just txOut -> error (name <> ": Unexpected output: " <> show txOut)
  traverse_ (uncurry check) [("refScriptPool", refScriptPool), ("refScriptSwap", refScriptSwap), ("refScriptDeposit", refScriptDeposit), ("refScriptRedeem", refScriptRedeem)]
