{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module Armadillo.ChainFollower.PoolState(
  PoolState(..),
  utxos,
  updatePoolState,

  PoolOutput(..),
  poolOutputFromTxOut,

  poolEvents
) where

import           Armadillo.Orphans      ()
import           Cardano.Api            (BlockInMode, CardanoMode, CtxTx,
                                         ScriptHash, SlotNo, TxIn)
import qualified Cardano.Api            as C
import qualified Cardano.Api.Shelley    as C
import           Control.Lens           (makeLensesFor)
import qualified Control.Lens           as L
import qualified Convex.Lenses          as L
import           Convex.Scripts         (fromScriptData)
import           Convex.Utxos           (UtxoChangeEvent, UtxoSet)
import qualified Convex.Utxos           as Utxos
import           Data.Aeson             (FromJSON, ToJSON)
import           ErgoDex.Contracts.Pool (PoolConfig (..))
import           GHC.Generics           (Generic)

data PoolOutput =
  PoolOutput
    { _poConfig :: PoolConfig
    , _poTxIn   :: TxIn
    , _poSlot   :: SlotNo
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data PoolState =
  PoolState
    { _utxos :: UtxoSet CtxTx PoolOutput
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Semigroup PoolState where
  l <> r =
    PoolState
      { _utxos = _utxos l <> _utxos r
      }

instance Monoid PoolState where
  mempty = PoolState mempty

makeLensesFor
  [ ("_utxos", "utxos")
  ] ''PoolState

{-| Extract a @PoolOutput@ from a tx output
-}
poolOutputFromTxOut :: ScriptHash -> SlotNo -> TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Maybe PoolOutput
poolOutputFromTxOut scriptHash _poSlot _poTxIn txo =
  let outputPaymentCredential = L.preview (L._TxOut . L._1 . L._AddressInEra . L._Address . L._2 . L.to C.fromShelleyPaymentCredential . L._PaymentCredentialByScript) txo
      outputDatum = L.preview (L._TxOut . L._3 . L._TxOutDatumInline) txo
  in case (outputPaymentCredential, outputDatum) of
        (Just opHash, Just (fromScriptData . C.getScriptData -> Just _poConfig))
          | opHash == scriptHash -> Just PoolOutput{_poConfig, _poSlot, _poTxIn}
          | otherwise -> Nothing
        _ -> Nothing

poolEvents :: ScriptHash -> UtxoSet C.CtxTx PoolOutput -> C.BlockInMode C.CardanoMode -> [Utxos.UtxoChangeEvent PoolOutput]
poolEvents credentials oldUtxo block =
  let C.BlockInMode (C.Block (C.BlockHeader slotNo _ _) _) _ = block
  in Utxos.extract (poolOutputFromTxOut credentials slotNo) Nothing oldUtxo block

updatePoolState :: ScriptHash -> BlockInMode CardanoMode -> PoolState -> (PoolState, [UtxoChangeEvent PoolOutput])
updatePoolState sh block state@PoolState{_utxos} =
  let newPoolEvents = poolEvents sh _utxos block
      newPoolState  = Utxos.apply _utxos (foldMap Utxos.fromEvent newPoolEvents)
  in (state{_utxos = newPoolState}, newPoolEvents)
