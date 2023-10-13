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

  -- * Making redemptions
  RedeemOutput(..),
  redeem,
  applyRedemption,

  -- * Making swaps
  SwapOutput(..),
  SwapParams(..),
  swap,
  applySwap,

  -- * Etc.
  poolValue,
  mintToken
) where

import           Armadillo.BuildTx.Deposit       (DepositOutput (..), deposit,
                                                  depositPairX, depositPairY)
import           Armadillo.BuildTx.Pool          (PoolInflows (..),
                                                  PoolLiquidity (..),
                                                  PoolLiquidityToken (..),
                                                  PoolNFT (..), PoolOutput (..),
                                                  PoolState (..),
                                                  RewardsAndChange (..),
                                                  addPoolOutput,
                                                  createPoolLiquidityToken,
                                                  createPoolNft, liquidity,
                                                  poolConfig,
                                                  poolLiquidityAssetId,
                                                  poolNftAssetId, poolState,
                                                  poolValue, poolXAssetId,
                                                  poolYAssetId, reservesX,
                                                  reservesY, rewardLp,
                                                  sharesAmount)
import           Armadillo.BuildTx.Redeem        (RedeemOutput (..),
                                                  amountRedeemed, redeem,
                                                  redeemReturnOutput)
import           Armadillo.BuildTx.Swap          (SwapOutput (..),
                                                  SwapParams (..), swap)
import           Armadillo.BuildTx.Types         (DEXBuildTxError (..),
                                                  ReferenceScripts (..),
                                                  deployReferenceScripts,
                                                  txOutValue)
import           Armadillo.Orphans               ()
import           Armadillo.Scripts               (Scripts (..))
import qualified Armadillo.Scripts               as Scripts
import           Cardano.Api                     (AssetName, NetworkId,
                                                  Quantity (..), ScriptHash,
                                                  TxIn)
import qualified Cardano.Api                     as C
import qualified Cardano.Api.Shelley             as C
import           Control.Monad.Except            (MonadError, throwError)
import           Convex.BuildTx                  (MonadBuildTx, mintPlutusV2,
                                                  prependTxOut,
                                                  setMinAdaDepositAll,
                                                  spendPlutusV2RefWithInlineDatum)
import           Convex.Class                    (MonadBlockchain (..))
import           Convex.PlutusLedger             (unTransAddressInEra)
import           Convex.Utils                    (mapError)
import           Convex.Wallet.Operator          (Operator, operatorAddress)
import qualified Data.Set                        as Set
import           ErgoDex.Contracts.Pool          (PoolConfig (..))
import qualified ErgoDex.Contracts.Pool          as Pool
import           ErgoDex.Contracts.Proxy.Deposit (DepositConfig)
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import qualified ErgoDex.Contracts.Proxy.Order   as Order
import qualified ErgoDex.Contracts.Proxy.Redeem  as R
import qualified PlutusLedgerApi.V1              as PV1

import qualified Debug.Trace                     as Trace

{-| Create a simple token
-}
mintToken :: MonadBuildTx m => AssetName -> Quantity -> m ScriptHash
mintToken name quantity = do
  mintPlutusV2 Scripts.v2MintingScript () name quantity
  pure $ C.hashScript $ C.PlutusScript C.PlutusScriptV2 Scripts.v2MintingScript

applyDeposit :: (MonadBuildTx m, MonadBlockchain m) => ReferenceScripts TxIn -> Scripts -> Operator k -> DepositOutput TxIn -> PoolOutput TxIn -> m (PoolOutput (), C.TxOut C.CtxTx C.BabbageEra)
applyDeposit ReferenceScripts{refScriptPool, refScriptDeposit} scripts@Scripts{sPoolValidator, sDepositValidator} op dep@DepositOutput{doCfg, doTxIn} poolOutput@PoolOutput{poConfig, poTxIn} = do
  (n, p) <- (,) <$> networkId <*> queryProtocolParameters
  let preIns = Set.fromList [doTxIn, poTxIn]
      orderIx = fromIntegral $ Set.findIndex doTxIn preIns
      poolIx = fromIntegral $ Set.findIndex poTxIn preIns
      D.DepositConfig{D.exFee, D.tokenA, D.collateralAda} = doCfg
      PoolConfig{poolX, poolY} = poConfig
      depositRed = Order.OrderRedeemer{Order.poolInIx = poolIx, Order.orderInIx = orderIx, Order.rewardOutIx = 1, Order.action = Order.Apply}
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

  Trace.traceM $ "inflows="          <> show inflows
  Trace.traceM $ "poolState="        <> show (poolState poolOutput)
  Trace.traceM $ "rewardsAndChange=" <> show rewardsAndChange

  spendPlutusV2RefWithInlineDatum doTxIn refScriptDeposit (Just $ fst sDepositValidator) depositRed
  spendPlutusV2RefWithInlineDatum poTxIn refScriptPool    (Just $ fst sPoolValidator)    poolRed

  let rwdOutput = rewardOutput n poConfig doCfg rewardsAndChange
  prependTxOut rwdOutput
  newPoolOutput <- addPoolOutput scripts poConfig newState
  setMinAdaDepositAll p
  let opRewardOut = operatorRewardOutput n op 0

  pure (newPoolOutput, opRewardOut)

applyRedemption :: (MonadBuildTx m, MonadBlockchain m, MonadError DEXBuildTxError m) => ReferenceScripts TxIn -> Scripts -> Operator k -> RedeemOutput TxIn -> PoolOutput TxIn -> m (PoolOutput (), C.TxOut C.CtxTx C.BabbageEra)
applyRedemption ReferenceScripts{refScriptPool, refScriptRedeem} scripts@Scripts{sPoolValidator, sRedeemValidator} op RedeemOutput{rdoCfg, rdoTxIn, rdoTxOut} poolOutput@PoolOutput{poConfig, poTxIn} = do
  (n, p) <- (,) <$> networkId <*> queryProtocolParameters
  let preIns = Set.fromList [rdoTxIn, poTxIn]
      orderIx = fromIntegral $ Set.findIndex rdoTxIn preIns
      poolIx = fromIntegral $ Set.findIndex poTxIn preIns
      poolRed   = Pool.PoolRedeemer{Pool.action = Pool.Redeem, Pool.selfIx = 0}
      redeemRed = Order.OrderRedeemer{Order.poolInIx = poolIx, Order.orderInIx = orderIx, Order.rewardOutIx = 1, Order.action = Order.Apply}
      R.RedeemConfig{R.exFee} = rdoCfg

      oldState = poolState poolOutput
      rwds@RewardsAndChange{rewards, changeX, changeY} = sharesAmount oldState (amountRedeemed poConfig $ txOutValue rdoTxOut)

      newState =
        oldState
          { liquidity = payRewards rewards (liquidity oldState)
          , reservesX = reservesX oldState - changeX
          , reservesY = reservesY oldState - changeY
          }

  spendPlutusV2RefWithInlineDatum rdoTxIn refScriptRedeem (Just $ fst sRedeemValidator) redeemRed
  spendPlutusV2RefWithInlineDatum poTxIn  refScriptPool   (Just $ fst sPoolValidator)   poolRed

  returnOut <- mapError CardanoApiError (redeemReturnOutput n poConfig rdoCfg (changeX, changeY))

  prependTxOut returnOut
  newPoolOutput <- addPoolOutput scripts poConfig newState

  Trace.traceM $ "rewardsAndChange= " <> show rwds
  Trace.traceM $ "poolState (old) = " <> show oldState
  Trace.traceM $ "poolState (new) = " <> show newState

  setMinAdaDepositAll p
  let opRewardOut = operatorRewardOutput n op (Quantity exFee)

  pure (newPoolOutput, opRewardOut)

applySwap :: (MonadBuildTx m, MonadBlockchain m, MonadError DEXBuildTxError m) => ReferenceScripts TxIn -> Scripts -> Operator k -> SwapOutput TxIn -> PoolOutput TxIn -> m (PoolOutput (), C.TxOut C.CtxTx C.BabbageEra)
applySwap ReferenceScripts{refScriptPool, refScriptSwap} Scripts{sPoolValidator, sSwapValidator} _op SwapOutput{swoTxIn} PoolOutput{poTxIn} = do
  (_n, p) <- (,) <$> networkId <*> queryProtocolParameters
  let preIns = Set.fromList [swoTxIn, poTxIn]
      orderIx = fromIntegral $ Set.findIndex swoTxIn preIns
      poolIx = fromIntegral $ Set.findIndex poTxIn preIns
      poolRed   = Pool.PoolRedeemer{Pool.action = Pool.Swap, Pool.selfIx = 0}
      orderRedeemer = Order.OrderRedeemer{Order.poolInIx = poolIx, Order.orderInIx = orderIx, Order.rewardOutIx = 1, Order.action = Order.Apply}

  spendPlutusV2RefWithInlineDatum swoTxIn refScriptSwap (Just $ fst sSwapValidator) orderRedeemer
  spendPlutusV2RefWithInlineDatum poTxIn refScriptPool (Just $ fst sPoolValidator) poolRed

  setMinAdaDepositAll p
  -- let opRewardOut = operatorRewardOutput n op 0 -- FIXME: Compute rewards (Quantity exFee)

  throwError NotImplemented


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
