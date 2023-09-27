{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-| Building transactions
-}
module Armadillo.BuildTx(
  ReferenceScripts(..),
  deployReferenceScripts,

  -- * Creating tokens
  PoolNFT(..),
  createPoolNft,
  PoolLiquidityToken(..),
  createPoolLiquidityToken,

  -- * Creating pools
  PoolOutput(..),
  poolConfig,
  addPoolOutput,

  -- * Etc.
  poolValue,
  mintToken
) where

import           Armadillo.Orphans        ()
import           Armadillo.Scripts        (CardanoApiScriptError, Scripts (..))
import qualified Armadillo.Scripts        as Scripts
import           Cardano.Api              (AssetId, AssetName, PolicyId,
                                           Quantity (..), ScriptHash, TxIn,
                                           Value)
import qualified Cardano.Api              as C
import qualified Cardano.Api.Shelley      as C
import           Control.Lens             (_2, over, preview)
import           Control.Monad.Except     (MonadError, liftEither, throwError)
import           Convex.BuildTx           (MonadBuildTx, addBtx, mintPlutusV2,
                                           prependTxOut, setMinAdaDeposit,
                                           spendPublicKeyOutput)
import           Convex.Class             (MonadBlockchain (..))
import qualified Convex.Lenses            as L
import           Convex.PlutusLedger      (transAssetId, transTxOutRef,
                                           unTransAssetId)
import           Convex.Scripts           (toHashableScriptData)
import           Convex.Wallet.Operator   (Operator)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Maybe               (fromMaybe)
import           ErgoDex.CardanoApi       (poolLqMintingScript,
                                           poolNftMintingScript)
import           ErgoDex.Contracts.Pool   (PoolConfig (..), PoolState (..))
import           GHC.Generics             (Generic)
import           PlutusLedgerApi.V1.Value (AssetClass)

data ReferenceScripts i =
  ReferenceScripts
    { refScriptPool    :: i
    , refScriptSwap    :: i
    , refScriptDeposit :: i
    , refScriptRedeem  :: i
    }
    deriving stock (Eq, Show, Functor, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Deploy the reference scripts
-}
deployReferenceScripts :: (MonadBuildTx m, MonadBlockchain m, MonadError CardanoApiScriptError m) => Operator k -> m (ReferenceScripts Int)
deployReferenceScripts _operator = do
  -- TODO: SCript spendable by operator
  Scripts{sPoolValidator, sSwapValidator, sDepositValidator, sRedeemValidator} <- liftEither Scripts.compileScripts
  let cred = C.PaymentCredentialByScript $ C.hashScript $ (C.PlutusScript C.PlutusScriptV2) Scripts.v2SpendingScript
  addr <- C.makeShelleyAddress <$> networkId <*> pure cred <*> pure C.NoStakeAddress
  let mkAndAddOutput = prependTxOut . mkRefScriptTxOut addr
  sequence_ $ mkAndAddOutput <$> [sPoolValidator, sSwapValidator, sSwapValidator, sDepositValidator, sRedeemValidator]
  pure
    ReferenceScripts
      { refScriptPool = 3
      , refScriptSwap = 2
      , refScriptDeposit = 1
      , refScriptRedeem = 0
      }

{-| A transaction output that has the given script as a reference script
-}
mkRefScriptTxOut :: C.Address C.ShelleyAddr -> C.PlutusScript C.PlutusScriptV2 -> C.TxOut ctx C.BabbageEra
mkRefScriptTxOut addr script =
  let vl = C.lovelaceToValue 0
  in C.TxOut (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr)
                (C.TxOutValue C.MultiAssetInBabbageEra vl)
                C.TxOutDatumNone
                (C.ReferenceScript C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (C.toScriptInAnyLang $ C.PlutusScript C.PlutusScriptV2 script))

data PoolNFT =
  PoolNFT
    { pnftAsset   :: (PolicyId, AssetName)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

createPoolNft :: (MonadBuildTx m, MonadError CardanoApiScriptError m) => TxIn -> m PoolNFT
createPoolNft txInput = do
  let tn = "pool nft"
  script <- case poolNftMintingScript (transTxOutRef txInput) "pool nft" of
    Right s  -> pure s
    Left err -> throwError err
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  mintPlutusV2 script () tn 1
  spendPublicKeyOutput txInput
  pure PoolNFT
    { pnftAsset = (C.PolicyId scriptHash, tn)
    }

data PoolLiquidityToken =
  PoolLiquidityToken
    { pltAsset   :: (PolicyId, AssetName)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

createPoolLiquidityToken :: (MonadBuildTx m, MonadError CardanoApiScriptError m) => TxIn -> Quantity -> m PoolLiquidityToken
createPoolLiquidityToken txInput q@(Quantity n) = do
  let tn = "liquidity"
  script <- case poolLqMintingScript (transTxOutRef txInput) "liquidity" n of
    Right s -> pure s
    Left x  -> throwError x
  let scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 script)
  mintPlutusV2 script () tn q
  spendPublicKeyOutput txInput
  pure PoolLiquidityToken
    { pltAsset = (C.PolicyId scriptHash, tn)
    }

-- | Add the pool output to a transaction
addPoolOutput :: (MonadBlockchain m, MonadBuildTx m, MonadError CardanoApiScriptError m) => PoolLiquidityToken -> PoolNFT -> PoolConfig -> PoolState -> m (C.TxOut C.CtxTx C.BabbageEra)
addPoolOutput poLiquidity poNFT poolCfg poolState = do
  n <- networkId
  protocolParameters <- queryProtocolParameters
  Scripts{sPoolValidator} <- liftEither Scripts.compileScripts

  let addr = C.makeShelleyAddressInEra n (C.PaymentCredentialByScript (C.hashScript $ C.PlutusScript C.PlutusScriptV2 sPoolValidator)) C.NoStakeAddress
      dat = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (toHashableScriptData poolCfg)
      value' = valueForState poLiquidity poNFT poolCfg poolState
  let txOut =
        setMinAdaDeposit protocolParameters
        $ C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value') dat C.ReferenceScriptNone -- TODO: Use a ref. script?

  addBtx $ over L.txOuts ((:) txOut)
  pure txOut

valueForState :: PoolLiquidityToken -> PoolNFT -> PoolConfig -> PoolState -> Value
valueForState liq nft cfg PoolState{reservesX, reservesY, liquidity} =
  C.valueFromList
    [ (poolNftAssetId nft, 1)
    , (poolLiquidityAssetId liq, Quantity liquidity)
    , (poolXAssetId cfg, Quantity reservesX)
    , (poolYAssetId cfg, Quantity reservesY)
    ]


txOutValue :: C.TxOut C.CtxTx C.BabbageEra -> C.Value
txOutValue = fromMaybe mempty . preview (L._TxOut . _2 . L._TxOutValue)

poolValue :: PoolOutput -> C.Value
poolValue = txOutValue . poTxOut

poolXAssetId :: PoolConfig -> AssetId
poolXAssetId = either (error . ((<>) "poolXAssetId: unTransAssetId failed: " . show)) id . unTransAssetId . poolX

poolYAssetId :: PoolConfig -> AssetId
poolYAssetId = either (error . ((<>) "poolYAssetId: unTransAssetId failed: " . show)) id . unTransAssetId . poolY

poolNftAssetId :: PoolNFT -> AssetId
poolNftAssetId = uncurry C.AssetId . pnftAsset

poolLiquidityAssetId :: PoolLiquidityToken -> AssetId
poolLiquidityAssetId = uncurry C.AssetId . pltAsset

data PoolOutput =
  PoolOutput
    { poTxOut     :: (C.TxOut C.CtxTx C.BabbageEra)
    , poConfig    :: PoolConfig
    , poState     :: PoolState
    , poNFT       :: PoolNFT
    , poLiquidity :: PoolLiquidityToken
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

poolConfig :: AssetClass -> AssetClass -> Integer -> PoolLiquidityToken -> PoolNFT -> PoolConfig
poolConfig poolX poolY poolFeeNum  PoolLiquidityToken{pltAsset} poolNFT =
  PoolConfig
    { poolNft = transAssetId $ poolNftAssetId poolNFT
    , poolX
    , poolY
    , poolLq = transAssetId $ uncurry C.AssetId pltAsset
    , poolFeeNum
    , stakeAdminPolicy = []
    , lqBound = 0
    }

{-| Create a simple token
-}
mintToken :: MonadBuildTx m => AssetName -> Quantity -> m ScriptHash
mintToken name quantity = do
  mintPlutusV2 Scripts.v2MintingScript () name quantity
  pure $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 Scripts.v2MintingScript
