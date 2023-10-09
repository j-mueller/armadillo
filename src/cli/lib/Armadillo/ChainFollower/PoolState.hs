{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Armadillo.ChainFollower.PoolState(
  PoolUtxoState(..),
  utxos,
  updatePoolState,
  poolOutputFromTxOut,

  poolEvents
) where

import           Armadillo.BuildTx   (PoolOutput (..))
import           Armadillo.Orphans   ()
import           Cardano.Api         (BlockInMode, CardanoMode, CtxTx,
                                      ScriptHash, SlotNo, TxIn)
import qualified Cardano.Api         as C
import qualified Cardano.Api.Shelley as C
import           Control.Lens        (makeLensesFor)
import qualified Control.Lens        as L
import qualified Convex.Lenses       as L
import           Convex.Scripts      (fromScriptData)
import           Convex.Utxos        (UtxoChangeEvent, UtxoSet)
import qualified Convex.Utxos        as Utxos
import           Data.Aeson          (FromJSON, ToJSON)
import           GHC.Generics        (Generic)

data PoolUtxoState =
  PoolUtxoState
    { _utxos :: UtxoSet CtxTx (PoolOutput TxIn)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Semigroup PoolUtxoState where
  l <> r =
    PoolUtxoState
      { _utxos = _utxos l <> _utxos r
      }

instance Monoid PoolUtxoState where
  mempty = PoolUtxoState mempty

makeLensesFor
  [ ("_utxos", "utxos")
  ] ''PoolUtxoState

{-| Extract a @PoolOutput@ from a tx output
-}
poolOutputFromTxOut :: ScriptHash -> SlotNo -> TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Maybe (PoolOutput TxIn)
poolOutputFromTxOut scriptHash _poSlot poTxIn poTxOut =
  let outputPaymentCredential = L.preview (L._TxOut . L._1 . L._AddressInEra . L._Address . L._2 . L.to C.fromShelleyPaymentCredential . L._PaymentCredentialByScript) poTxOut
      outputDatum = L.preview (L._TxOut . L._3 . L._TxOutDatumInline) poTxOut
  in case (outputPaymentCredential, outputDatum) of
        (Just opHash, Just (fromScriptData . C.getScriptData -> Just poConfig))
          | opHash == scriptHash -> Just PoolOutput{poTxOut, poConfig, poTxIn}
          | otherwise -> Nothing
        _ -> Nothing

poolEvents :: ScriptHash -> UtxoSet C.CtxTx (PoolOutput TxIn) -> C.BlockInMode C.CardanoMode -> [Utxos.UtxoChangeEvent (PoolOutput TxIn)]
poolEvents credentials oldUtxo block =
  let C.BlockInMode (C.Block (C.BlockHeader slotNo _ _) _) _ = block
  in Utxos.extract (poolOutputFromTxOut credentials slotNo) Nothing oldUtxo block

updatePoolState :: ScriptHash -> BlockInMode CardanoMode -> PoolUtxoState -> (PoolUtxoState, [UtxoChangeEvent (PoolOutput TxIn)])
updatePoolState sh block state@PoolUtxoState{_utxos} =
  let newPoolEvents = poolEvents sh _utxos block
      newPoolState  = Utxos.apply _utxos (foldMap Utxos.fromEvent newPoolEvents)
  in (state{_utxos = newPoolState}, newPoolEvents)
