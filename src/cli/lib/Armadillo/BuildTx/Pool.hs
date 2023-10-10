{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
module Armadillo.BuildTx.Pool(
  PoolOutput(..),
  PoolInflows(..),
  RewardsAndChange(..),
  PoolLiquidity(..),
  PoolNFT(..),
  PoolLiquidityToken(..),
  PoolState(..),
  poolConfig,
  createPoolLiquidityToken,
  createPoolNft,
  rewardLp,
  sharesAmount,
  poolState,
  addPoolOutput,
  poolLiquidityAssetId,
  poolNftAssetId,
  poolXAssetId,
  poolYAssetId,
  poolValue
) where

import           Armadillo.BuildTx.Types  (txOutValue)
import           Armadillo.Orphans        ()
import           Armadillo.Scripts        (CardanoApiScriptError, Scripts (..))
import           Cardano.Api              (AssetId, AssetName, PolicyId,
                                           Quantity (..), TxIn, Value)
import qualified Cardano.Api              as C
import qualified Cardano.Api.Shelley      as C
import           Control.Lens             (over)
import           Control.Monad.Except     (MonadError, throwError)
import           Convex.BuildTx           (MonadBuildTx, addBtx, mintPlutusV2,
                                           setMinAdaDeposit,
                                           spendPublicKeyOutput)
import           Convex.Class             (MonadBlockchain (..))
import qualified Convex.Lenses            as L
import           Convex.PlutusLedger      (transAssetId, transTxOutRef,
                                           unTransAssetId)
import           Convex.Scripts           (toHashableScriptData)
import           Data.Aeson               (FromJSON, ToJSON)
import           ErgoDex.CardanoApi       (poolLqMintingScript,
                                           poolNftMintingScript)
import           ErgoDex.Contracts.Pool   (PoolConfig (..))
import qualified ErgoDex.Contracts.Pool   as Pool
import           ErgoDex.Contracts.Types  (Amount (..))
import           GHC.Generics             (Generic)
import           PlutusLedgerApi.V1.Value (AssetClass)

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

{-| The number of tokens returned to the user in exchange for the given number
of liquidity tokens
-}
sharesAmount :: PoolState -> Quantity -> RewardsAndChange
sharesAmount PoolState{reservesX, reservesY, liquidity=PoolLiquidity{plLiquidity}} burnedLq =
  RewardsAndChange
    { rewards = negate burnedLq
    , changeX = burnedLq * reservesX `div` plLiquidity
    , changeY = burnedLq * reservesY `div` plLiquidity
    }

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

data PoolInflows =
  PoolInflows
    { netInX :: Quantity
    , netInY :: Quantity
    }
    deriving Show
