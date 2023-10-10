{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Armadillo.BuildTx.Deposit(
  DepositOutput(..),
  deposit,
  depositPairX,
  depositPairY
) where

import           Armadillo.BuildTx.Types         (DEXBuildTxError (..),
                                                  txOutValue)
import           Armadillo.Orphans               ()
import           Armadillo.Scripts               (Scripts (..))
import           Cardano.Api                     (AssetId, Quantity (..), Value)
import qualified Cardano.Api                     as C
import qualified Cardano.Api.Shelley             as C
import           Control.Monad.Except            (MonadError, liftEither)
import           Convex.BuildTx                  (MonadBuildTx, minAdaDeposit,
                                                  prependTxOut)
import           Convex.Class                    (MonadBlockchain (..))
import           Convex.PlutusLedger             (transPubKeyHash,
                                                  transStakeKeyHash,
                                                  unTransAssetId)
import           Convex.Scripts                  (toHashableScriptData)
import           Convex.Utils                    (mapError)
import           Data.Aeson                      (FromJSON, ToJSON)
import           ErgoDex.Contracts.Pool          (PoolConfig (..))
import           ErgoDex.Contracts.Proxy.Deposit (DepositConfig)
import qualified ErgoDex.Contracts.Proxy.Deposit as D
import           GHC.Generics                    (Generic)

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
