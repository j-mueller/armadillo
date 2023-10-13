{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-identities #-}
{-# LANGUAGE DeriveAnyClass    #-}
module Armadillo.Orphans() where

import           Cardano.Api                     (Quantity (..))
import qualified Codec.CBOR.Write                as Write
import qualified Codec.Serialise                 as Serialise
import           Codec.Serialise.Class           (Serialise)
import           Control.Monad                   ((>=>))
import           Data.Aeson                      (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Types                as JSON
import           Data.Bifunctor                  (Bifunctor (..))
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Base16          as Base16
import qualified Data.ByteString.Lazy            as BSL
import           Data.OpenApi                    (ToSchema (..))
import           Data.Proxy                      (Proxy (..))
import           Data.String                     (IsString (..))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as TE
import           ErgoDex.Contracts.Pool          (PoolConfig (..),
                                                  PoolState (..))
import           ErgoDex.Contracts.Proxy.Deposit (DepositConfig (..))
import           ErgoDex.Contracts.Proxy.Redeem  (RedeemConfig (..))
import           ErgoDex.Contracts.Proxy.Swap    (SwapConfig (..))
import           GHC.Generics                    (Generic)
import           PlutusCore.Data                 (Data)
import           PlutusLedgerApi.V1.Address      (Address)
import           PlutusLedgerApi.V1.Credential   (Credential, StakingCredential)
import           PlutusLedgerApi.V1.Crypto       (PubKeyHash)
import qualified PlutusLedgerApi.V1.Scripts      as P
import           PlutusLedgerApi.V1.Tx           (TxId (..), TxOutRef (..))
import           PlutusLedgerApi.V1.Value        (AssetClass (..),
                                                  CurrencySymbol (..),
                                                  TokenName (..), Value)
import qualified PlutusTx
import qualified PlutusTx.AssocMap               as AssocMap
import qualified PlutusTx.Prelude                as PlutusTx
import           Servant.API                     (FromHttpApiData (..))
import           Text.Read                       (readMaybe)

deriving newtype instance Enum Quantity
deriving newtype instance Real Quantity
deriving newtype instance Integral Quantity

deriving stock instance Eq PoolState
deriving stock instance Eq DepositConfig
deriving stock instance Eq RedeemConfig
deriving stock instance Eq SwapConfig

deriving stock instance Generic PoolState
deriving stock instance Generic PoolConfig

deriving anyclass instance ToJSON PoolState
deriving anyclass instance ToJSON PoolConfig
deriving newtype instance ToJSON AssetClass
deriving anyclass instance ToJSON DepositConfig
deriving anyclass instance ToJSON RedeemConfig
deriving anyclass instance ToJSON SwapConfig

deriving anyclass instance FromJSON PoolState
deriving anyclass instance FromJSON PoolConfig
deriving newtype instance FromJSON AssetClass
deriving anyclass instance FromJSON DepositConfig
deriving anyclass instance FromJSON RedeemConfig
deriving anyclass instance FromJSON SwapConfig

deriving anyclass instance (ToJSON k, ToJSON v) => ToJSON (AssocMap.Map k v)
deriving anyclass instance (FromJSON k, FromJSON v) => FromJSON (AssocMap.Map k v)

deriving anyclass instance FromJSON Address
deriving anyclass instance ToJSON Address

deriving anyclass instance FromJSON CurrencySymbol
deriving anyclass instance ToJSON CurrencySymbol

deriving anyclass instance FromJSON Value
deriving anyclass instance ToJSON Value

deriving anyclass instance FromJSON Credential
deriving anyclass instance ToJSON Credential

deriving anyclass instance FromJSON StakingCredential
deriving anyclass instance ToJSON StakingCredential

deriving anyclass instance FromJSON TokenName
deriving anyclass instance ToJSON TokenName

deriving anyclass instance FromJSON P.ScriptHash
deriving anyclass instance ToJSON P.ScriptHash

deriving anyclass instance FromJSON P.Datum
deriving anyclass instance ToJSON P.Datum

deriving anyclass instance FromJSON P.DatumHash
deriving anyclass instance ToJSON P.DatumHash

deriving anyclass instance FromJSON TxOutRef
deriving anyclass instance ToJSON TxOutRef

deriving anyclass instance ToJSON TxId
deriving anyclass instance FromJSON TxId
deriving anyclass instance JSON.ToJSONKey TxId
deriving anyclass instance JSON.FromJSONKey TxId

instance ToJSON PlutusTx.BuiltinByteString where
    toJSON = JSON.String . encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
    parseJSON v = PlutusTx.toBuiltin <$> decodeByteString v

instance ToJSON PlutusTx.BuiltinData where
  toJSON = toJSON . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinData where
  parseJSON v = parseJSON v >>= pure . PlutusTx.dataToBuiltinData

deriving anyclass instance ToJSON PubKeyHash
deriving anyclass instance FromJSON PubKeyHash
deriving anyclass instance JSON.FromJSONKey PubKeyHash
deriving anyclass instance JSON.ToJSONKey PubKeyHash

instance ToSchema PubKeyHash where
    declareNamedSchema _ = declareNamedSchema (Proxy @Text)

deriving via (JSONViaSerialise Data) instance ToJSON Data
deriving via (JSONViaSerialise Data) instance FromJSON Data

-- | Newtype for deriving 'ToJSON' and 'FromJSON' for types that have a 'Serialise'
-- instance by just encoding the serialized bytes as a JSON string.
newtype JSONViaSerialise a = JSONViaSerialise a

instance Serialise a => ToJSON (JSONViaSerialise a) where
    toJSON (JSONViaSerialise a) = JSON.String $ encodeSerialise a

instance Serialise a => FromJSON (JSONViaSerialise a) where
    parseJSON v = JSONViaSerialise <$> decodeSerialise v

encodeByteString :: BS.ByteString -> Text.Text
encodeByteString = TE.decodeUtf8 . Base16.encode

tryDecode :: Text.Text -> Either String BS.ByteString
tryDecode = Base16.decode . TE.encodeUtf8

decodeByteString :: JSON.Value -> JSON.Parser BS.ByteString
decodeByteString = JSON.withText "ByteString" (either fail pure . tryDecode)

encodeSerialise :: Serialise a => a -> Text.Text
encodeSerialise = encodeByteString . Write.toStrictByteString . Serialise.encode

decodeSerialise :: Serialise a => JSON.Value -> JSON.Parser a
decodeSerialise = decodeByteString >=> go where
    go bs =
        case first show $ Serialise.deserialiseOrFail $ BSL.fromStrict bs of
            Left e  -> fail e
            Right v -> pure v

instance FromHttpApiData TxOutRef where
    parseUrlPiece x = case Text.splitOn txOutRefSep x of
        [hash, index] -> case readMaybe (Text.unpack index) of
            Just i  -> Right (TxOutRef (fromString $ Text.unpack hash) i)
            Nothing -> Left $ "Failed to parse index: " <> index
        _             -> Left ("Failed to parse txOutRef: " <> x)

txOutRefSep :: Text
txOutRefSep = ":"
