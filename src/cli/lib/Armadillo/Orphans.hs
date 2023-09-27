{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
module Armadillo.Orphans() where

import           Data.Aeson                  (FromJSON (..), ToJSON (..))
import           Data.ByteString.Base64.Type (getByteString64, makeByteString64)
import           ErgoDex.Contracts.Pool      (PoolConfig (..), PoolState (..))
import           GHC.Generics                (Generic)
import           PlutusLedgerApi.V1.Value    (AssetClass (..),
                                              CurrencySymbol (..),
                                              TokenName (..))
import           PlutusTx.Builtins           (BuiltinByteString, fromBuiltin,
                                              toBuiltin)

instance ToJSON BuiltinByteString where
  toJSON = toJSON . makeByteString64 . fromBuiltin

instance FromJSON BuiltinByteString where
  parseJSON = fmap (toBuiltin . getByteString64) . parseJSON

deriving stock instance Eq PoolState
deriving stock instance Generic PoolState

deriving stock instance Generic PoolConfig

deriving anyclass instance ToJSON PoolState
deriving anyclass instance ToJSON PoolConfig
deriving newtype instance ToJSON CurrencySymbol
deriving newtype instance ToJSON AssetClass
deriving newtype instance ToJSON TokenName

deriving anyclass instance FromJSON PoolState
deriving anyclass instance FromJSON PoolConfig
deriving newtype instance FromJSON CurrencySymbol
deriving newtype instance FromJSON AssetClass
deriving newtype instance FromJSON TokenName
