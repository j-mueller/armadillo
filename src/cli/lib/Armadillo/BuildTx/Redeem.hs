{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Armadillo.BuildTx.Redeem(
  RedeemOutput(..),
  redeem,
  amountRedeemed,
  redeemReturnOutput
) where

import           Armadillo.BuildTx.Pool         (poolLiquidityAssetId)
import           Armadillo.BuildTx.Types        (DEXBuildTxError (..))
import           Armadillo.Orphans              ()
import           Armadillo.Scripts              (Scripts (..))
import           Cardano.Api                    (NetworkId, Quantity (..),
                                                 Value)
import qualified Cardano.Api                    as C
import qualified Cardano.Api.Shelley            as C
import           Control.Monad.Except           (MonadError, liftEither)
import           Convex.BuildTx                 (MonadBuildTx, prependTxOut)
import           Convex.Class                   (MonadBlockchain (..))
import           Convex.PlutusLedger            (transPubKeyHash,
                                                 transStakeKeyHash,
                                                 unTransAddressInEra,
                                                 unTransAssetId)
import           Convex.Scripts                 (toHashableScriptData)
import           Convex.Utils                   (mapError)
import           Data.Aeson                     (FromJSON, ToJSON)
import           ErgoDex.Contracts.Pool         (PoolConfig (..))
import           ErgoDex.Contracts.Proxy.Redeem (RedeemConfig)
import qualified ErgoDex.Contracts.Proxy.Redeem as R
import           GHC.Generics                   (Generic)
import qualified PlutusLedgerApi.V1             as PV1

data RedeemOutput txi =
  RedeemOutput
    { rdoTxIn  :: txi
    , rdoTxOut :: C.TxOut C.CtxTx C.BabbageEra
    , rdoCfg   :: RedeemConfig
    }
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (ToJSON, FromJSON)

redeem :: (MonadBuildTx m, MonadError DEXBuildTxError m, MonadBlockchain m) => Scripts -> C.Hash C.PaymentKey -> Maybe (C.Hash C.StakeKey) -> PoolConfig -> Quantity -> m (RedeemOutput ())
redeem scripts verificationKey stakeKey cfg@PoolConfig{poolNft, poolX, poolY, poolLq} quantity = do
  n <- networkId
  value <- mapError CardanoApiError (redeemValue cfg quantity)
  let redeemConfig0 =
        R.RedeemConfig
          { R.poolNft = poolNft
          , R.poolX = poolX
          , R.poolY = poolY
          , R.poolLp = poolLq
          , R.exFee = 2_000_000 -- FIXME: Configurable fee
          , R.rewardPkh = transPubKeyHash verificationKey
          , R.stakePkh = transStakeKeyHash <$> stakeKey
          }
      txOut = redeemTxOut scripts n redeemConfig0 value
  prependTxOut txOut
  pure $ RedeemOutput () txOut redeemConfig0

{-| The @cardano-api@ value containing the liquidity tokens to be returned to a liquidity pool.
-}
redeemValue :: MonadError C.SerialiseAsRawBytesError m => PoolConfig -> Quantity -> m Value
redeemValue PoolConfig{poolLq} quantity = do
  lq <- liftEither (unTransAssetId poolLq)

  -- We need to include enough Ada to run the deposit & pool validators
  -- because the deposit validator only allows 2 inputs (so we can't have)
  -- a 3rd Ada-only input)
  let minAda = C.Lovelace 10_000_000
  pure $ C.valueFromList [(lq, quantity)] <> C.lovelaceToValue minAda

{-| The tx output for a token redemption
-}
redeemTxOut :: Scripts -> C.NetworkId -> RedeemConfig -> Value -> C.TxOut C.CtxTx C.BabbageEra
redeemTxOut Scripts{sRedeemValidator} n cfg value =
  let addr = C.makeShelleyAddressInEra n (C.PaymentCredentialByScript (fst sRedeemValidator)) C.NoStakeAddress
      dat = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (toHashableScriptData cfg)
  in (C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra value) dat C.ReferenceScriptNone)

amountRedeemed :: PoolConfig -> Value -> Quantity
amountRedeemed cfg vl =
  C.selectAsset vl (poolLiquidityAssetId cfg)

redeemReturnOutput :: MonadError C.SerialiseAsRawBytesError m => NetworkId -> PoolConfig -> RedeemConfig -> (Quantity, Quantity) -> m (C.TxOut C.CtxTx C.BabbageEra)
redeemReturnOutput n PoolConfig{poolX, poolY} R.RedeemConfig{R.rewardPkh, R.stakePkh} (amountX, amountY) = do
  aX <- liftEither (unTransAssetId poolX)
  aY <- liftEither (unTransAssetId poolY)
  addr <- liftEither $ unTransAddressInEra n $ PV1.Address (PV1.PubKeyCredential rewardPkh) (fmap (PV1.StakingHash . PV1.PubKeyCredential) stakePkh)
  let vl = C.valueFromList [(aX, amountX), (aY, amountY)]
  pure $ C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra vl) C.TxOutDatumNone C.ReferenceScriptNone
