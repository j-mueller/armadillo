{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Armadillo.Test.Scripts(
  mintNativeToken,
  validatorScript
) where

import           Armadillo.Test.Validator       (alwaysSucceedsValidator)
import           Cardano.Api                    (AssetId (..), AssetName,
                                                 PlutusScriptV2, Quantity)
import qualified Cardano.Api.Shelley            as C
import           Control.Monad                  (void)
import           Convex.BuildTx                 (execBuildTx', mintPlutusV1)
import           Convex.Class                   (MonadMockchain)
import           Convex.MockChain.CoinSelection (balanceAndSubmit, paymentTo)
import           Convex.Scripts                 (compiledCodeToScript)
import           Convex.Wallet                  (Wallet)
import qualified Convex.Wallet.MockWallet       as Wallet
import           PlutusTx                       (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude               (BuiltinData)

{-| Mint a number of native tokens with the given asset name
-}
mintNativeToken :: (MonadFail m, MonadMockchain m) => Wallet -> AssetName -> Quantity -> m (AssetId, Quantity)
mintNativeToken wallet assetName q = do
  void $ Wallet.w3 `paymentTo` wallet
  let mintingScript = C.examplePlutusScriptAlwaysSucceeds C.WitCtxMint
      tx = execBuildTx' (mintPlutusV1 mintingScript () assetName q)
  void $ balanceAndSubmit wallet tx
  let hsh = C.hashScript $ C.PlutusScript C.PlutusScriptV1 mintingScript
  pure (C.AssetId (C.PolicyId hsh) assetName, q)

validatorScriptCompiled :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorScriptCompiled =
  $$(PlutusTx.compile [|| \d r c -> alwaysSucceedsValidator d r c ||])

{-| The alwaysSucceeds validator script
-}
validatorScript :: C.PlutusScript PlutusScriptV2
validatorScript = compiledCodeToScript validatorScriptCompiled
