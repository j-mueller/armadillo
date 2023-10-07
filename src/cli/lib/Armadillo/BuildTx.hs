{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-| Building transactions
-}
module Armadillo.BuildTx(
  DEXBuildTxError(..),
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
  poolXAssetId,
  poolYAssetId,
  poolNftAssetId,
  poolLiquidityAssetId,

  -- * Making deposits
  Deposit(..),
  deposit,

  -- * Etc.
  poolValue,
  mintToken
) where

import           Armadillo.Orphans               ()
import           Armadillo.Scripts               (CardanoApiScriptError,
                                                  Scripts (..))
import qualified Armadillo.Scripts               as Scripts
import           Cardano.Api                     (AssetId, AssetName, PolicyId,
                                                  Quantity (..), ScriptHash,
                                                  TxIn, Value)
import qualified Cardano.Api                     as C
import qualified Cardano.Api.Shelley             as C
import           Control.Lens                    (_2, over, preview)
import           Control.Monad.Except            (MonadError, liftEither,
                                                  throwError)
import           Convex.BuildTx                  (MonadBuildTx, addBtx,
                                                  minAdaDeposit, mintPlutusV2,
                                                  prependTxOut,
                                                  setMinAdaDeposit,
                                                  spendPublicKeyOutput)
import           Convex.Class                    (MonadBlockchain (..))
import qualified Convex.Lenses                   as L
import           Convex.PlutusLedger             (transAssetId, transPubKeyHash,
                                                  transStakeKeyHash,
                                                  transTxOutRef, unTransAssetId)
import           Convex.Scripts                  (toHashableScriptData)
import           Convex.Utils                    (mapError)
import           Convex.Wallet.Operator          (Operator)
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Maybe                      (fromMaybe)
import           ErgoDex.CardanoApi              (poolLqMintingScript,
                                                  poolNftMintingScript)
import           ErgoDex.Contracts.Pool          (PoolConfig (..),
                                                  PoolState (..))
import           ErgoDex.Contracts.Proxy.Deposit (DepositConfig)
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import           GHC.Generics                    (Generic)
import           PlutusLedgerApi.V1.Value        (AssetClass)

data ReferenceScripts i =
  ReferenceScripts
    { refScriptPool    :: i
    , refScriptSwap    :: i
    , refScriptDeposit :: i
    , refScriptRedeem  :: i
    }
    deriving stock (Eq, Show, Functor, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DEXBuildTxError =
  BuildTxScriptError CardanoApiScriptError
  | CardanoApiError C.SerialiseAsRawBytesError
  deriving Show

{-| Deploy the reference scripts
-}
deployReferenceScripts :: (MonadBuildTx m, MonadBlockchain m) => Scripts -> Operator k -> m (ReferenceScripts Int)
deployReferenceScripts Scripts{sPoolValidator, sSwapValidator, sDepositValidator, sRedeemValidator} _operator = do
  let cred = C.PaymentCredentialByScript $ C.hashScript $ (C.PlutusScript C.PlutusScriptV2) Scripts.v2SpendingScript
  addr <- C.makeShelleyAddress <$> networkId <*> pure cred <*> pure C.NoStakeAddress
  let mkAndAddOutput = prependTxOut . mkRefScriptTxOut addr . snd
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
addPoolOutput :: (MonadBlockchain m, MonadBuildTx m) => Scripts -> PoolConfig -> PoolState -> m (PoolOutput ())
addPoolOutput Scripts{sPoolValidator} poolCfg poolState = do
  n <- networkId
  protocolParameters <- queryProtocolParameters

  let addr = C.makeShelleyAddressInEra n (C.PaymentCredentialByScript (C.hashScript $ C.PlutusScript C.PlutusScriptV2 $ snd sPoolValidator)) C.NoStakeAddress
      dat = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (toHashableScriptData poolCfg)
      value' = valueForState poolCfg poolState
  let txOut =
        setMinAdaDeposit protocolParameters
        $ C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value') dat C.ReferenceScriptNone -- TODO: Use a ref. script?

  addBtx $ over L.txOuts ((:) txOut)
  pure PoolOutput{poTxOut = txOut, poConfig = poolCfg, poTxIn = ()}

valueForState :: PoolConfig -> PoolState -> Value
valueForState cfg PoolState{reservesX, reservesY, liquidity} =
  C.valueFromList
    [ (poolNftAssetId cfg, 1)
    , (poolLiquidityAssetId cfg, Quantity liquidity)
    , (poolXAssetId cfg, Quantity reservesX)
    , (poolYAssetId cfg, Quantity reservesY)
    ]

txOutValue :: C.TxOut C.CtxTx C.BabbageEra -> C.Value
txOutValue = fromMaybe mempty . preview (L._TxOut . _2 . L._TxOutValue)

poolValue :: PoolOutput i -> C.Value
poolValue = txOutValue . poTxOut

poolXAssetId :: PoolConfig -> AssetId
poolXAssetId = either (error . ((<>) "poolXAssetId: unTransAssetId failed: " . show)) id . unTransAssetId . poolX

poolYAssetId :: PoolConfig -> AssetId
poolYAssetId = either (error . ((<>) "poolYAssetId: unTransAssetId failed: " . show)) id . unTransAssetId . poolY

poolNftAssetId :: PoolConfig -> AssetId
poolNftAssetId = either (error . ((<>) "poolNftAssetId: unTransAssetId failed: " . show)) id . unTransAssetId . poolNft

poolLiquidityAssetId :: PoolConfig -> AssetId
poolLiquidityAssetId = either (error . ((<>) "poolLiquidityAssetId: unTransAssetId failed: " . show)) id . unTransAssetId . poolLq

data PoolOutput txi =
  PoolOutput
    { poTxOut  :: (C.TxOut C.CtxTx C.BabbageEra)
    , poConfig :: PoolConfig
    , poTxIn   :: txi
    }
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (ToJSON, FromJSON)

poolConfig :: AssetClass -> AssetClass -> Integer -> PoolLiquidityToken -> PoolNFT -> PoolConfig
poolConfig poolX poolY poolFeeNum  PoolLiquidityToken{pltAsset} PoolNFT{pnftAsset} =
  PoolConfig
    { poolNft = transAssetId $ uncurry C.AssetId pnftAsset
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

data Deposit =
  Deposit
    { doTxOut         :: C.TxOut C.CtxTx C.BabbageEra
    , doDepositConfig :: DepositConfig
    , doPoolConfig    :: PoolConfig
    , doValue         :: Value
    , doAssetX        :: Quantity
    , doAssetY        :: Quantity
    }

deposit :: (MonadBuildTx m, MonadError DEXBuildTxError m, MonadBlockchain m) => Scripts -> C.Hash C.PaymentKey -> Maybe (C.Hash C.StakeKey) -> PoolConfig -> Quantity -> m Deposit
deposit scripts verificationKey stakeKey cfg@PoolConfig{poolNft, poolX, poolY, poolLq} quantity = do
  n <- networkId
  protParams <- queryProtocolParameters
  value <- mapError CardanoApiError (depositValue cfg quantity)
  let depositCfg0 =
        D.DepositConfig
          { D.poolNft = poolNft
          , D.tokenA = poolX
          , D.tokenB = poolY
          , D.tokenLp = poolLq
          , D.exFee = 2_000_000 -- FIXME calculate fee
          , D.rewardPkh = transPubKeyHash verificationKey
          , D.stakePkh = transStakeKeyHash <$> stakeKey
          , D.collateralAda = 2_000_000
          }
  let output0 = depositTxOut scripts n depositCfg0 value
  let Quantity minAda = minAdaDeposit protParams output0
  let depositCfg1 = depositCfg0{D.collateralAda = minAda}
  let output1 = depositTxOut scripts n depositCfg1 (value <> C.lovelaceToValue (C.Lovelace minAda))
  addBtx $ over L.txOuts ((:) output1)
  pure $ Deposit output1 depositCfg1 cfg value quantity quantity

{-| The @cardano-api@ value containing the tokens to be deposited to a liquidity pool.
-}
depositValue :: MonadError C.SerialiseAsRawBytesError m => PoolConfig -> Quantity -> m Value
depositValue PoolConfig{poolX, poolY} quantity = do
  aX <- liftEither (unTransAssetId poolX)
  aY <- liftEither (unTransAssetId poolY)

  -- We need to include enough Ada to run the deposit & pool validators
  -- because the deposit validator only allows 2 inputs (so we can't have)
  -- a 3rd Ada-only input)
  let minAda = C.Lovelace 20_000_000
  pure $ C.valueFromList [(aX, quantity), (aY, quantity)] <> C.lovelaceToValue minAda

{-| The tx output for a token deposit
-}
depositTxOut :: Scripts -> C.NetworkId -> DepositConfig -> Value -> C.TxOut C.CtxTx C.BabbageEra
depositTxOut Scripts{sDepositValidator} n cfg value =
  let addr = C.makeShelleyAddressInEra n (C.PaymentCredentialByScript (fst sDepositValidator)) C.NoStakeAddress
      dat = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (toHashableScriptData cfg)
  in (C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value) dat C.ReferenceScriptNone)
