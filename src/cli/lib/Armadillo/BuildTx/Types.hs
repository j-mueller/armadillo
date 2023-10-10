{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Armadillo.BuildTx.Types(
  DEXBuildTxError(..),
  ReferenceScripts(..),
  deployReferenceScripts,
  txOutValue
) where

import           Armadillo.Scripts      (CardanoApiScriptError, Scripts (..))
import qualified Armadillo.Scripts      as Scripts
import qualified Cardano.Api            as C
import qualified Cardano.Api.Shelley    as C
import           Control.Lens           (_2, preview)
import           Convex.BuildTx         (MonadBuildTx, prependTxOut)
import           Convex.Class           (MonadBlockchain (..))
import qualified Convex.Lenses          as L
import           Convex.Wallet.Operator (Operator)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Maybe             (fromMaybe)
import           GHC.Generics           (Generic)

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

txOutValue :: C.TxOut C.CtxTx C.BabbageEra -> C.Value
txOutValue = fromMaybe mempty . preview (L._TxOut . _2 . L._TxOutValue)
