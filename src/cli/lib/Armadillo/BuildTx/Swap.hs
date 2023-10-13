{-# LANGUAGE DeriveAnyClass #-}
module Armadillo.BuildTx.Swap(
  SwapOutput(..),
  SwapParams(..),
  swap
) where

import           Armadillo.Orphans            ()
import           Armadillo.Scripts            (Scripts (..))
import           Cardano.Api                  (Quantity (..), Value)
import qualified Cardano.Api                  as C
import qualified Cardano.Api.Shelley          as C
import           Convex.BuildTx               (MonadBuildTx, prependTxOut,
                                               setMinAdaDepositAll)
import           Convex.Class                 (MonadBlockchain (..))
import qualified Convex.PlutusLedger          as PL
import qualified Convex.Scripts               as Scripts
import           Data.Aeson                   (FromJSON, ToJSON)
import           ErgoDex.Contracts.Pool       (PoolConfig)
import qualified ErgoDex.Contracts.Pool       as P
import           ErgoDex.Contracts.Proxy.Swap (SwapConfig (..))
import           GHC.Generics                 (Generic)

data SwapOutput txi =
  SwapOutput
    { swoTxIn  :: txi
    , swoTxOut :: C.TxOut C.CtxTx C.BabbageEra
    , swoCfg   :: SwapConfig
    }
    deriving stock (Eq, Show, Generic, Functor)
    deriving anyclass (ToJSON, FromJSON)

data SwapParams =
  SwapParams
    { spBase             :: C.AssetId
    , spQuote            :: C.AssetId
    , spExFeePerTokenNum :: Integer
    , spExFeePerTokenDen :: Integer
    , spRewardPkh        :: C.Hash C.PaymentKey
    , spStakePkh         :: Maybe (C.Hash C.StakeKey)
    , spBaseAmount       :: Integer
    , spMinQuoteAmount   :: Integer
    }
    deriving stock (Eq, Show, Generic)

swap :: (MonadBuildTx m, MonadBlockchain m) => Scripts -> PoolConfig -> SwapParams -> m (SwapOutput ())
swap scripts poolCfg params@SwapParams{spBase, spQuote, spExFeePerTokenNum, spExFeePerTokenDen, spRewardPkh, spStakePkh, spBaseAmount, spMinQuoteAmount} = do
  n <- networkId
  -- FIXME: Basic validation (asset IDs should match those in the pool)
  let cfg =
        SwapConfig
          { base  = PL.transAssetId spBase
          , quote = PL.transAssetId spQuote
          , poolNft = P.poolNft poolCfg
          , feeNum = P.poolFeeNum poolCfg
          , exFeePerTokenNum = spExFeePerTokenNum
          , exFeePerTokenDen = spExFeePerTokenDen
          , rewardPkh = PL.transPubKeyHash spRewardPkh
          , stakePkh = PL.transStakeKeyHash <$> spStakePkh
          , baseAmount = spBaseAmount
          , minQuoteAmount = spMinQuoteAmount
          }
      txOut = swapOutput scripts n cfg (swapValue params)
  prependTxOut txOut
  queryProtocolParameters >>= setMinAdaDepositAll
  pure $ SwapOutput () txOut cfg

{-| The value that needs to be locked in the swap output
-}
swapValue :: SwapParams -> C.Value
swapValue SwapParams{spBase, spBaseAmount} =
  C.valueFromList [(spBase, Quantity spBaseAmount)]

swapOutput :: Scripts -> C.NetworkId -> SwapConfig -> Value -> C.TxOut C.CtxTx C.BabbageEra
swapOutput Scripts{sSwapValidator} n cfg vl =
  let addr = C.makeShelleyAddressInEra n (C.PaymentCredentialByScript (fst sSwapValidator)) C.NoStakeAddress
      dat  = C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra (Scripts.toHashableScriptData cfg)
  in (C.TxOut addr (C.TxOutValue C.MultiAssetInBabbageEra vl) dat C.ReferenceScriptNone)
