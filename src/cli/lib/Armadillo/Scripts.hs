{-# LANGUAGE GADTs            #-}
{-# OPTIONS_GHC -fobject-code -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-} -- 1.1.0.0 will be enabled in conway
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
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
  v2MintingScriptSerialised,

  -- * Loading scripts from serialised UPLC
  ScriptsConfig(..),
  loadScriptsConfig,
  scriptsFromScriptsConfig,
  readValidatorFromFile
) where

import           Cardano.Api                   (ScriptHash)
import qualified Cardano.Api                   as C
import qualified Cardano.Api.Shelley           as C
import           Cardano.Binary                (DecoderError)
import           Control.Monad.Except          (runExceptT)
import           Control.Monad.IO.Class        (MonadIO (..))
import           Convex.Utils                  (liftEither)
import qualified Data.ByteString.Lazy          as BSL
import           Data.ByteString.Short         (toShort)
import           Dhall                         (ToDhall)
import           ErgoDex.CardanoApi            (CardanoApiScriptError,
                                                poolLqMintingScript,
                                                poolNftMintingScript)
import qualified ErgoDex.CardanoApi            as E
import           GHC.Generics                  (Generic)
import qualified Paths_armadillo_cli           as Pkg
import           PlutusLedgerApi.Common        (SerialisedScript)
import           PlutusLedgerApi.Test.Examples (alwaysSucceedingNAryFunction)
import           System.FilePath               ((</>))


-- | Plutus scripts that we need for the dex
data Scripts =
  Scripts
    { sPoolValidator    :: (ScriptHash, C.PlutusScript C.PlutusScriptV2)
    , sSwapValidator    :: (ScriptHash, C.PlutusScript C.PlutusScriptV2)
    , sDepositValidator :: (ScriptHash, C.PlutusScript C.PlutusScriptV2)
    , sRedeemValidator  :: (ScriptHash, C.PlutusScript C.PlutusScriptV2)
    }
    deriving Show

compileScripts :: Either CardanoApiScriptError Scripts
compileScripts =
  let mkH s = (C.hashScript (C.PlutusScript C.PlutusScriptV2 s), s) in
  Scripts
    <$> fmap mkH E.poolScript
    <*> fmap mkH E.swapScript
    <*> fmap mkH E.depositScript
    <*> fmap mkH E.redeemScript

v2SpendingScript :: C.PlutusScript C.PlutusScriptV2
v2SpendingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 3

v2SpendingScriptSerialised :: SerialisedScript
v2SpendingScriptSerialised = alwaysSucceedingNAryFunction 3

v2MintingScript :: C.PlutusScript C.PlutusScriptV2
v2MintingScript = C.PlutusScriptSerialised $ alwaysSucceedingNAryFunction 2

v2MintingScriptSerialised :: SerialisedScript
v2MintingScriptSerialised = alwaysSucceedingNAryFunction 2

data ScriptsConfig = ScriptsConfig
  { swapScriptPath    :: !FilePath
  , depositScriptPath :: !FilePath
  , redeemScriptPath  :: !FilePath
  , poolV1ScriptPath  :: !FilePath
  , poolV2ScriptPath  :: !FilePath
  } deriving (Generic, ToDhall)

loadScriptsConfig :: IO ScriptsConfig
loadScriptsConfig =
  let getScriptFile nm = Pkg.getDataFileName ("data" </> "scripts" </> nm <> ".uplc")
  in ScriptsConfig
      <$> getScriptFile "swap"
      <*> getScriptFile "deposit"
      <*> getScriptFile "redeem"
      <*> getScriptFile "poolV1"
      <*> getScriptFile "poolV2"

scriptsFromScriptsConfig :: ScriptsConfig -> IO (Either DecoderError Scripts)
scriptsFromScriptsConfig ScriptsConfig{swapScriptPath, depositScriptPath, redeemScriptPath, poolV2ScriptPath} = runExceptT $ do
  let mkH k@(C.PlutusScript _ s) = (C.hashScript k, s)
      mkH C.SimpleScript{} = error "scriptsFromScriptsConfig: impossible: SimpleScript"
      load = fmap mkH . liftEither id . liftIO . readValidatorFromFile
  Scripts
    <$> load poolV2ScriptPath
    <*> load swapScriptPath
    <*> load depositScriptPath
    <*> load redeemScriptPath

readValidatorFromFile :: FilePath -> IO (Either DecoderError (C.Script C.PlutusScriptV2))
readValidatorFromFile file = do
    bytes <- BSL.readFile file
    let s = C.PlutusScriptSerialised $ toShort $ BSL.toStrict bytes
    pure (Right $ C.PlutusScript C.PlutusScriptV2 s)
