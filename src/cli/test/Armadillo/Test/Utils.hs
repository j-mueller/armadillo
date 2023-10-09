{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Armadillo.Test.Utils(
  checkRefScripts,
  checkScriptHashes,
  availableTokens,
  scriptsFromScriptsConfig,
  loadScripts,
  checkRefScriptsUTxO
) where

import           Armadillo.BuildTx          (ReferenceScripts (..))
import           Armadillo.Scripts          (Scripts (..), loadScriptsConfig,
                                             readValidatorFromFile,
                                             scriptsFromScriptsConfig)
import           Armadillo.Test.AMMExecutor (ScriptsConfig (..))
import           Armadillo.Test.CliCommand  (loadRefScripts)
import           Cardano.Api                (AssetId, Quantity, ScriptHash,
                                             TxIn)
import qualified Cardano.Api                as C
import qualified Cardano.Api.Shelley        as C
import qualified Control.Lens               as L
import           Control.Monad.IO.Class     (MonadIO (..))
import           Convex.Devnet.CardanoNode  (RunningNode (..))
import           Convex.Devnet.Logging      (Tracer, traceWith)
import           Convex.Devnet.NodeQueries  (queryUTxOWhole)
import           Convex.Devnet.WalletServer (RunningWalletServer,
                                             WalletLog (..), getUTxOs)
import qualified Convex.Lenses              as L
import           Convex.Utxos               (totalBalance)
import           Data.Foldable              (traverse_)
import qualified Data.Map                   as Map
import           Test.Tasty.HUnit           (assertEqual)

checkRefScripts :: RunningNode -> FilePath -> IO ()
checkRefScripts RunningNode{rnNodeSocket, rnNetworkId} fp = do
  scripts <- loadRefScripts fp
  actualScripts <- loadScripts
  utxo <- queryUTxOWhole rnNetworkId rnNodeSocket
  checkRefScriptsUTxO scripts utxo actualScripts

{-| Check the reference scripts against the UTxO set
-}
checkRefScriptsUTxO :: ReferenceScripts TxIn -> C.UTxO era -> Scripts -> IO ()
checkRefScriptsUTxO ReferenceScripts{refScriptPool, refScriptSwap, refScriptDeposit, refScriptRedeem} (C.UTxO utxo) Scripts{sPoolValidator, sSwapValidator, sDepositValidator, sRedeemValidator} = do
  let check :: String -> (TxIn, (ScriptHash, C.PlutusScript C.PlutusScriptV2)) -> IO ()
      check name (txI, (hsh, _)) =
        case Map.lookup txI utxo of
          Nothing -> error (name <> ": Failed to find reference script")
          Just (L.view (L._TxOut . L._4) -> C.ReferenceScript _ (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) script))  -> do
            let hsh' = C.hashScript script
            if hsh' == hsh
              then pure ()
              else error (name <> ": Hash mismatch: Expected " <> show hsh <> ", found " <> show hsh')
          Just txOut -> error (name <> ": Unexpected output: " <> show txOut)
  traverse_ (uncurry check)
    [ ("refScriptPool",    (refScriptPool, sPoolValidator))
    , ("refScriptSwap",    (refScriptSwap, sSwapValidator))
    , ("refScriptDeposit", (refScriptDeposit, sDepositValidator))
    , ("refScriptRedeem",  (refScriptRedeem, sRedeemValidator))
    ]

checkScriptHashes :: Scripts -> ScriptsConfig -> IO ()
checkScriptHashes Scripts{sPoolValidator, sDepositValidator} ScriptsConfig{depositScriptPath, poolV2ScriptPath} = do
  readValidatorFromFile' poolV2ScriptPath >>=
    assertEqual "pool validator" (fst sPoolValidator) . C.hashScript
  readValidatorFromFile' depositScriptPath >>=
      assertEqual "deposit validator" (fst sDepositValidator) . C.hashScript

{-| Return the amount of tokens of the @AssetId@ that the wallet has
-}
availableTokens :: Tracer IO WalletLog -> RunningWalletServer -> AssetId -> IO Quantity
availableTokens tracer rws assetId = do
  totalBal <- totalBalance <$> getUTxOs rws
  traceWith tracer (WMsgText $ "Total balance: " <> C.renderValuePretty totalBal)
  let result = C.selectAsset totalBal assetId
  traceWith tracer (WMsgText $ "Asset ID: " <> C.renderValuePretty (C.valueFromList [(assetId, 1000)]))
  pure result

readValidatorFromFile' :: FilePath -> IO (C.Script C.PlutusScriptV2)
readValidatorFromFile' fp =
  readValidatorFromFile fp >>= either (error . (<>) ("ReadValidatorFromFile: " <> fp <> " :") . show) pure

loadScripts :: (MonadFail m, MonadIO m) => m Scripts
loadScripts = liftIO loadScriptsConfig >>= liftIO . scriptsFromScriptsConfig >>= either (fail . (<>) "Failed to load scripts: " . show) pure
