{-# LANGUAGE DeriveAnyClass #-}
{-| Building transactions
-}
module Armadillo.BuildTx(
  ReferenceScripts(..),
  deployReferenceScripts
) where

import           Armadillo.Scripts      (CardanoApiScriptError, Scripts (..))
import qualified Armadillo.Scripts      as Scripts
import qualified Cardano.Api            as C
import qualified Cardano.Api.Shelley    as C
import           Control.Monad.Except   (MonadError, liftEither)
import           Convex.BuildTx         (MonadBuildTx, prependTxOut)
import           Convex.Class           (MonadBlockchain (..))
import           Convex.Wallet.Operator (Operator, operatorAddress)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)

data ReferenceScripts i =
  ReferenceScripts
    { refScriptPool :: i
    }
    deriving stock (Eq, Show, Functor, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Deploy the reference scripts
-}
deployReferenceScripts :: (MonadBuildTx m, MonadBlockchain m, MonadError CardanoApiScriptError m) => Operator k -> m (ReferenceScripts Int)
deployReferenceScripts operator = do
  Scripts{sPoolValidator} <- liftEither Scripts.compileScripts
  addr <- operatorAddress <$> networkId <*> pure operator
  let out1 = mkRefScriptTxOut addr sPoolValidator
  prependTxOut out1
  pure
    ReferenceScripts
      { refScriptPool = 0
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
