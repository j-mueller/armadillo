{-# LANGUAGE TypeApplications #-}
module Armadillo.Scripts(
  Scripts(..),
  compileScripts,

  poolNftMintingScript,
  poolLqMintingScript,
  CardanoApiScriptError(..)
) where

import qualified Cardano.Api        as C
import           ErgoDex.CardanoApi (CardanoApiScriptError, poolLqMintingScript,
                                     poolNftMintingScript)
import qualified ErgoDex.CardanoApi as E

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
