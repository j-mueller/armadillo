{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
{-| Deposits
-}
module Armadillo.ChainFollower.DepositState(
  DepositOutput(..),
  DepositState(..),
  depositUtxos,

  depositOutFromTxOut,
  depositEvents,
  updateDepositState
) where

import           Armadillo.Orphans               ()
import           Cardano.Api                     (BlockInMode, CardanoMode,
                                                  CtxTx, ScriptHash, SlotNo,
                                                  TxIn)
import qualified Cardano.Api                     as C
import qualified Cardano.Api.Shelley             as C
import           Control.Lens                    (makeLensesFor)
import qualified Control.Lens                    as L
import qualified Convex.Lenses                   as L
import           Convex.Scripts                  (fromScriptData)
import           Convex.Utxos                    (UtxoChangeEvent, UtxoSet)
import qualified Convex.Utxos                    as Utxos
import           Data.Aeson                      (FromJSON, ToJSON)
import           ErgoDex.Contracts.Proxy.Deposit (DepositConfig)
import           GHC.Generics                    (Generic)

data DepositOutput =
  DepositOutput
    { _doCfg  :: DepositConfig
    , _doTxIn :: TxIn
    , _doSlot :: SlotNo
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

data DepositState =
  DepositState
    { _depositUtxos :: UtxoSet CtxTx DepositOutput
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Semigroup DepositState where
  l <> r =
    DepositState
      { _depositUtxos = _depositUtxos l <> _depositUtxos r
      }

instance Monoid DepositState where
  mempty = DepositState mempty

makeLensesFor
  [ ("_depositUtxos", "depositUtxos")
  ] ''DepositState

{-| Extract a @PoolOutput@ from a tx output
-}
depositOutFromTxOut :: ScriptHash -> SlotNo -> TxIn -> C.TxOut C.CtxTx C.BabbageEra -> Maybe DepositOutput
depositOutFromTxOut scriptHash _doSlot _doTxIn txo =
  let outputPaymentCredential = L.preview (L._TxOut . L._1 . L._AddressInEra . L._Address . L._2 . L.to C.fromShelleyPaymentCredential . L._PaymentCredentialByScript) txo
      outputDatum = L.preview (L._TxOut . L._3 . L._TxOutDatumInline) txo
  in case (outputPaymentCredential, outputDatum) of
        (Just opHash, Just (fromScriptData . C.getScriptData -> Just _doCfg))
          | opHash == scriptHash -> Just DepositOutput{_doCfg, _doSlot, _doTxIn}
          | otherwise -> Nothing
        _ -> Nothing

depositEvents :: ScriptHash -> UtxoSet C.CtxTx DepositOutput -> C.BlockInMode C.CardanoMode -> [Utxos.UtxoChangeEvent DepositOutput]
depositEvents credentials oldUtxo block =
  let C.BlockInMode (C.Block (C.BlockHeader slotNo _ _) _) _ = block
  in Utxos.extract (depositOutFromTxOut credentials slotNo) Nothing oldUtxo block

updateDepositState :: ScriptHash -> BlockInMode CardanoMode -> DepositState -> (DepositState, [UtxoChangeEvent DepositOutput])
updateDepositState sh block state@DepositState{_depositUtxos} =
  let newPoolEvents = depositEvents sh _depositUtxos block
      newDepositState  = Utxos.apply _depositUtxos (foldMap Utxos.fromEvent newPoolEvents)
  in (state{_depositUtxos = newDepositState}, newPoolEvents)
