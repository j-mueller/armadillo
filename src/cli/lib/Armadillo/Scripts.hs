{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-} -- 1.1.0.0 will be enabled in conway
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
module Armadillo.Scripts(
  Scripts(..),
  compileScripts,

  poolNftMintingScript,
  poolLqMintingScript,
  CardanoApiScriptError(..),

  v2SpendingScriptSerialised,
  v2SpendingScript,

  v2MintingScript,
  v2MintingScriptSerialised
) where

import qualified Cardano.Api                   as C
import qualified Cardano.Api.Shelley           as C
import           ErgoDex.CardanoApi            (CardanoApiScriptError,
                                                poolLqMintingScript,
                                                poolNftMintingScript)
import qualified ErgoDex.CardanoApi            as E
import           PlutusLedgerApi.Common        (SerialisedScript)
import           PlutusLedgerApi.Test.Examples (alwaysSucceedingNAryFunction)


-- | Plutus scripts that we need for the dex
data Scripts =
  Scripts
    { sPoolValidator    :: C.PlutusScript C.PlutusScriptV2
    , sSwapValidator    :: C.PlutusScript C.PlutusScriptV2
    , sDepositValidator :: C.PlutusScript C.PlutusScriptV2
    , sRedeemValidator  :: C.PlutusScript C.PlutusScriptV2
    }
    deriving Show

compileScripts :: Either CardanoApiScriptError Scripts
compileScripts =
  Scripts
    <$> E.poolScript
    <*> E.swapScript
    <*> E.depositScript
    <*> E.redeemScript

v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 3

v2SpendingScriptSerialised :: SerialisedScript
v2SpendingScriptSerialised = alwaysSucceedingNAryFunction 3

v2MintingScript :: C.PlutusScript C.PlutusScriptV2
v2MintingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 2

v2MintingScriptSerialised :: SerialisedScript
v2MintingScriptSerialised = alwaysSucceedingNAryFunction 2
