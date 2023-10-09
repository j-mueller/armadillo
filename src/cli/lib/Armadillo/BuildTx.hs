{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
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
  PoolState(..),
  PoolLiquidity(..),
  poolConfig,
  addPoolOutput,
  initialLiquidity,
  poolXAssetId,
  poolYAssetId,
  poolNftAssetId,
  poolLiquidityAssetId,

  -- * Making deposits
  DepositOutput(..),
  deposit,
  applyDeposit,

  -- * Etc.
  poolValue,
  mintToken
) where

import           Armadillo.Orphans               ()
import           Armadillo.Scripts               (CardanoApiScriptError,
                                                  Scripts (..))
import qualified Armadillo.Scripts               as Scripts
import           Cardano.Api                     (AssetId, AssetName, NetworkId,
                                                  PolicyId, Quantity (..),
                                                  ScriptHash, TxIn, Value)
import qualified Cardano.Api                     as C
import qualified Cardano.Api.Shelley             as C
import           Control.Lens                    (_2, over, preview)
import           Control.Monad.Except            (MonadError, liftEither,
                                                  throwError)
import           Convex.BuildTx                  (MonadBuildTx, addBtx,
                                                  minAdaDeposit, mintPlutusV2,
                                                  prependTxOut,
                                                  setMinAdaDeposit,
                                                  setMinAdaDepositAll,
                                                  spendPlutusV2RefWithInlineDatum,
                                                  spendPublicKeyOutput)
import           Convex.Class                    (MonadBlockchain (..))
import qualified Convex.Lenses                   as L
import           Convex.PlutusLedger             (transAssetId, transPubKeyHash,
                                                  transStakeKeyHash,
                                                  transTxOutRef,
                                                  unTransAddressInEra,
                                                  unTransAssetId)
import           Convex.Scripts                  (toHashableScriptData)
import           Convex.Utils                    (mapError)
import           Convex.Wallet.Operator          (Operator, operatorAddress)
import           Data.Aeson                      (FromJSON, ToJSON)
import           Data.Maybe                      (fromMaybe)
import           ErgoDex.CardanoApi              (poolLqMintingScript,
                                                  poolNftMintingScript)
import           ErgoDex.Contracts.Pool          (PoolConfig (..))
import qualified ErgoDex.Contracts.Pool          as Pool
import           ErgoDex.Contracts.Proxy.Deposit (DepositConfig)
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import qualified ErgoDex.Contracts.Proxy.Order   as Order
import           ErgoDex.Contracts.Types         (Amount (..))
import           GHC.Generics                    (Generic)
import qualified PlutusLedgerApi.V1              as PV1
import           PlutusLedgerApi.V1.Value        (AssetClass)

import qualified Debug.Trace                     as Trace

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
  sequence_ $ mkAndAddOutput <$> [sPoolValidator, sSwapValidator, sDepositValidator, sRedeemValidator]
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
addPoolOutput Scripts{sPoolValidator} poolCfg poolState_ = do
  n <- networkId
  protocolParameters <- queryProtocolParameters

  let addr = C.makeShelleyAddressInEra n (C.PaymentCredentialByScript (C.hashScript $ C.PlutusScript C.PlutusScriptV2 $ snd sPoolValidator)) C.NoStakeAddress
      dat = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (toHashableScriptData poolCfg)
      value' = valueForState poolCfg poolState_
  let txOut =
        setMinAdaDeposit protocolParameters
        $ C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value') dat C.ReferenceScriptNone -- TODO: Use a ref. script?

  addBtx $ over L.txOuts ((:) txOut)
  pure PoolOutput{poTxOut = txOut, poConfig = poolCfg, poTxIn = ()}

valueForState :: PoolConfig -> PoolState -> Value
valueForState cfg PoolState{reservesX, reservesY, liquidity=PoolLiquidity{plLockedInOutput}} =
  C.valueFromList
    [ (poolNftAssetId cfg, 1)
    , (poolLiquidityAssetId cfg, plLockedInOutput)
    , (poolXAssetId cfg, reservesX)
    , (poolYAssetId cfg, reservesY)
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

{-| The amount of liquidity tokens in the pool
-}
poolLiquidity :: PoolOutput i -> PoolLiquidity
poolLiquidity po@PoolOutput{poConfig} =
  let plLockedInOutput = C.selectAsset (poolValue po) (poolLiquidityAssetId poConfig)
  in PoolLiquidity
      { plLiquidity = Quantity (unAmount Pool.maxLqCapAmount) - plLockedInOutput
      , plLockedInOutput
      }

{-| Liquidity that has been used already, and remaining amounts
-}
data PoolLiquidity =
  PoolLiquidity
    { plLockedInOutput :: Quantity -- rlq
    , plLiquidity      :: Quantity -- lq
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

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

{-| Liquidity rewards and any change returned to the user
-}
data RewardsAndChange =
  RewardsAndChange
    { rewards :: Quantity
    , changeX :: Quantity
    , changeY :: Quantity
    }
    deriving stock Show

rewardLp :: PoolState -> PoolInflows -> RewardsAndChange
rewardLp PoolState{reservesX=Quantity resX, reservesY=Quantity resY, liquidity} PoolInflows{netInX = Quantity inX, netInY = Quantity inY} =
  let PoolLiquidity{plLiquidity=Quantity lq} = liquidity
      minByX = inX * lq `div` resX
      minByY = inY * lq `div` resY
      (Quantity -> changeX, Quantity -> changeY) =
        if (minByX == minByY)
          then (0, 0)
          else if minByX < minByY
            then (0, (minByY - minByX) * resY `div` lq)
            else ((minByX - minByY) * resX `div` lq, 0)
  in RewardsAndChange{ changeX, changeY, rewards = Quantity (min minByY minByX)}

{-| Create a simple token
-}
mintToken :: MonadBuildTx m => AssetName -> Quantity -> m ScriptHash
mintToken name quantity = do
  mintPlutusV2 Scripts.v2MintingScript () name quantity
  pure $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 Scripts.v2MintingScript

data DepositOutput txi =
  DepositOutput
    { doCfg   :: DepositConfig
    , doTxIn  :: txi
    , doTxOut :: C.TxOut C.CtxTx C.BabbageEra
    }
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (ToJSON, FromJSON)

depositPairX :: MonadError C.SerialiseAsRawBytesError m => DepositOutput i -> m (AssetId, Quantity)
depositPairX DepositOutput{doCfg=D.DepositConfig{D.tokenA}, doTxOut} = do
  assetId <- liftEither (unTransAssetId tokenA)
  pure (assetId, C.selectAsset (txOutValue doTxOut) assetId)

depositPairY :: MonadError C.SerialiseAsRawBytesError m => DepositOutput i -> m (AssetId, Quantity)
depositPairY DepositOutput{doCfg=D.DepositConfig{D.tokenB}, doTxOut} = do
  assetId <- liftEither (unTransAssetId tokenB)
  pure (assetId, C.selectAsset (txOutValue doTxOut) assetId)

deposit :: (MonadBuildTx m, MonadError DEXBuildTxError m, MonadBlockchain m) => Scripts -> C.Hash C.PaymentKey -> Maybe (C.Hash C.StakeKey) -> PoolConfig -> Quantity -> m (DepositOutput ())
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
  prependTxOut output1
  pure $ DepositOutput depositCfg1 () output1

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

data PoolInflows =
  PoolInflows
    { netInX :: Quantity
    , netInY :: Quantity
    }
    deriving Show

applyDeposit :: (MonadBuildTx m, MonadBlockchain m) => ReferenceScripts TxIn -> Scripts -> Operator k -> DepositOutput TxIn -> PoolOutput TxIn -> m (PoolOutput (), C.TxOut C.CtxTx C.BabbageEra)
applyDeposit ReferenceScripts{refScriptPool, refScriptDeposit} scripts@Scripts{sPoolValidator, sDepositValidator} op dep@DepositOutput{doCfg, doTxIn} poolOutput@PoolOutput{poConfig, poTxIn} = do
  (n, p) <- (,) <$> networkId <*> queryProtocolParameters
  let D.DepositConfig{D.exFee, D.tokenA, D.collateralAda} = doCfg
      PoolConfig{poolX, poolY} = poConfig
      depositRed = Order.OrderRedeemer{Order.poolInIx = 0, Order.orderInIx = 1, Order.rewardOutIx = 1, Order.action = Order.Apply}
      poolRed    = Pool.PoolRedeemer{Pool.action = Pool.Deposit, Pool.selfIx = 0}

  (inX, inY) <-
      if (tokenA == poolX)
        then either (error . show) pure ((,) <$> depositPairX dep <*> depositPairY dep)
        else either (error . show) pure ((,) <$> depositPairY dep <*> depositPairX dep)


  let
      inflows@PoolInflows{netInX, netInY}
        | Order.isAda poolX = PoolInflows{netInX = snd inX - Quantity exFee - Quantity collateralAda, netInY = snd inY}
        | Order.isAda poolY = PoolInflows{netInX = snd inX, netInY = snd inY - Quantity exFee - Quantity collateralAda}
        | otherwise       = PoolInflows{netInX = snd inX, netInY = snd inY}

      rewardsAndChange@RewardsAndChange{rewards} = rewardLp (poolState poolOutput) inflows

      oldState = poolState poolOutput
      newState =
        oldState
          { liquidity = payRewards rewards (liquidity oldState)
          , reservesX = reservesX oldState + netInX
          , reservesY = reservesY oldState + netInY
          }

  Trace.traceM $ "inflows=" <> show inflows
  Trace.traceM $ "poolState=" <> show (poolState poolOutput)
  Trace.traceM $ "rewardsAndChange=" <> show rewardsAndChange

  spendPlutusV2RefWithInlineDatum doTxIn refScriptDeposit (Just $ fst sDepositValidator) depositRed
  spendPlutusV2RefWithInlineDatum poTxIn refScriptPool    (Just $ fst sPoolValidator)    poolRed

  let rwdOutput = rewardOutput n poConfig doCfg rewardsAndChange
  prependTxOut rwdOutput
  newPoolOutput <- addPoolOutput scripts poConfig newState
  setMinAdaDepositAll p
  let opRewardOut = operatorRewardOutput n op 0

  pure (newPoolOutput, opRewardOut)

rewardOutput :: NetworkId -> PoolConfig -> DepositConfig -> RewardsAndChange -> C.TxOut C.CtxTx C.BabbageEra
rewardOutput n poolCfg D.DepositConfig{D.rewardPkh, D.stakePkh} RewardsAndChange{rewards, changeX, changeY} =
  let addr = either (error "unTransAddressInEra") id $ unTransAddressInEra n $ PV1.Address (PV1.PubKeyCredential rewardPkh) (fmap (PV1.StakingHash . PV1.PubKeyCredential) stakePkh)
      amount =
        C.valueFromList
          [ (poolLiquidityAssetId poolCfg, rewards)
          , (poolXAssetId poolCfg, changeX)
          , (poolYAssetId poolCfg, changeY)
          ]
  in C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra amount) C.TxOutDatumNone C.ReferenceScriptNone

operatorRewardOutput :: NetworkId -> Operator k -> Quantity -> C.TxOut C.CtxTx C.BabbageEra
operatorRewardOutput n operator q =
  let addr = C.shelleyAddressInEra (operatorAddress n operator)
      amount = C.valueFromList [(C.AdaAssetId, q)]
  in C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra amount) C.TxOutDatumNone C.ReferenceScriptNone

assetXReserves :: PoolOutput i -> Quantity
assetXReserves po@PoolOutput{poConfig} = C.selectAsset (poolValue po) (poolXAssetId poConfig)

assetYReserves :: PoolOutput i -> Quantity
assetYReserves po@PoolOutput{poConfig} = C.selectAsset (poolValue po) (poolYAssetId poConfig)

poolState :: PoolOutput i -> PoolState
poolState po =
  PoolState
    { reservesX = assetXReserves po
    , reservesY = assetYReserves po
    , liquidity = poolLiquidity po
    }

data PoolState = PoolState
    { reservesX :: Quantity
    , reservesY :: Quantity
    , liquidity :: PoolLiquidity
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

payRewards :: Quantity -> PoolLiquidity -> PoolLiquidity
payRewards rwds pl =
  PoolLiquidity
    { plLockedInOutput = plLockedInOutput pl - rwds
    , plLiquidity = plLiquidity pl + rwds
    }

initialLiquidity :: Quantity -> PoolLiquidity
initialLiquidity q =
  PoolLiquidity
    {plLockedInOutput = Quantity Pool.maxLqCap - q
    , plLiquidity = q
    }
