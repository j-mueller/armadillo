{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-| @kupo@ integration
-}
module Armadillo.Kupo(
  KupoUtxoQueryT(..),
  runKupoUtxoQueryT,

  -- * Config
  KupoConfig(..),
  parseKupoConfig,
  kupoClientEnv
) where

import           Armadillo.Utils        (readAssetId)
import           Cardano.Api            (Lovelace (..), PaymentCredential,
                                         Quantity (..), TxId, TxIn (..),
                                         TxIx (..), UTxO (..))
import qualified Cardano.Api            as C
import qualified Cardano.Api.Shelley    as C
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..), ReaderT, runReaderT)
import           Convex.Class           (MonadBlockchain (..))
import           Convex.MonadLog        (MonadLog (..))
import           Convex.Query           (MonadUtxoQuery (..))
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Bifunctor         (bimap)
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Proxy             (Proxy (..))
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import           GHC.Generics           (Generic)
import qualified Network.HTTP.Client    as HTTP
import           Options.Applicative    (Parser, auto, help, long, option,
                                         strOption)
import qualified Options.Applicative
import           Servant.API            (Capture, Get, JSON, ToHttpApiData (..),
                                         type (:>))
import           Servant.Client         (BaseUrl (..), ClientEnv, ClientError,
                                         Scheme (Http), client, mkClientEnv,
                                         runClientM)

newtype KupoCredentialMatch = KupoCredentialMatch PaymentCredential

instance ToHttpApiData KupoCredentialMatch where
  toUrlPiece (KupoCredentialMatch credential) = case credential of
  -- when translating the payment credential to a kupo pattern we need to add the
  -- wildcard to match all staking credentials
  -- https://cardanosolutions.github.io/kupo/#section/Patterns/Credential
    C.PaymentCredentialByKey keyhash -> C.serialiseToRawBytesHexText keyhash <> "/*"
    C.PaymentCredentialByScript scripthash -> C.serialiseToRawBytesHexText scripthash <> "/*"

newtype KupoAssets = KupoAssets (Map Text Integer)
  deriving stock (Show)
  deriving newtype (ToJSON, FromJSON)

data KupoValue =
  KupoValue
    { coins  :: Integer
    , assets :: KupoAssets
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

fromKupoAsset :: Text -> Integer -> Either KupoConversionError (C.AssetId, C.Quantity)
fromKupoAsset t i = do
  bimap (FailedToDecodeAssetId . Text.pack) (, Quantity i) (readAssetId '.' (Text.unpack t))

fromKupoValue :: KupoValue -> Either KupoConversionError C.Value
fromKupoValue KupoValue{coins, assets=KupoAssets mp} = do
  cAssets <- traverse (uncurry fromKupoAsset) (Map.toList mp)
  pure (C.lovelaceToValue (Lovelace coins) <> C.valueFromList cAssets)

data KupoOutput =
  KupoOutput
    { transaction_index :: Integer
    , transaction_id    :: Text
    , output_index      :: Word
    , address           :: Text
    , value             :: KupoValue
    }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type KupoAPI =
  "matches" :> Capture "credential" KupoCredentialMatch :> Get '[JSON] [KupoOutput]

getKupoMatches :: ClientEnv -> PaymentCredential -> IO (Either ClientError [KupoOutput])
getKupoMatches clientEnv cred = do
  let matches = client (Proxy @KupoAPI)
  runClientM (matches $ KupoCredentialMatch cred) clientEnv

data KupoConversionError =
  KupoConversionError
  | FailedToDeserialiseAddress Text
  | FailedToDeserialiseTxId Text C.RawBytesHexError
  | FailedToDecodeAssetId Text
  deriving stock Show

fromKupoOutput :: KupoOutput -> Either KupoConversionError (C.TxIn, C.TxOut C.CtxUTxO C.BabbageEra)
fromKupoOutput KupoOutput{transaction_id, output_index, address, value} = do
  addr <- maybe (Left $ FailedToDeserialiseAddress address) pure (C.deserialiseAddress C.AsShelleyAddress address)
  txId <- either (Left . FailedToDeserialiseTxId address) pure (C.deserialiseFromRawBytesHex (C.proxyToAsType $ Proxy @TxId) $ Text.encodeUtf8 transaction_id)
  txVal <- fromKupoValue value
  let txIn = TxIn txId (TxIx output_index)
  pure (txIn, C.TxOut (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr) (C.TxOutValue C.MultiAssetInBabbageEra txVal) C.TxOutDatumNone C.ReferenceScriptNone)

newtype KupoUtxoQueryT m a = KupoUtxoQueryT { unKupoUtxoQueryT :: ReaderT ClientEnv m a }
  deriving newtype (Functor, Applicative, Monad, MonadLog, MonadIO, MonadBlockchain)

deriving newtype instance MonadError e m => MonadError e (KupoUtxoQueryT m)

instance MonadIO m => MonadUtxoQuery (KupoUtxoQueryT m) where
  utxosByPayment cred = KupoUtxoQueryT $ do
    env <- ask
    results <- liftIO (getKupoMatches env cred) >>= \case
      Left err -> do
        liftIO $ putStrLn $ "KupoUtxoQueryT: utxosByPayment: request failed: " <> show err
        error (show err)
      Right x -> pure x
    case traverse fromKupoOutput results of
      Left err -> do
        liftIO $ putStrLn $ "KupoUtxoQueryT: utxosByPayment: fromKupoOutput failed: " <> show err
        error (show err)
      Right k -> pure $ UTxO $ Map.fromList k

{-| Run the @KupoUtxoQueryT@, making calls to a kupo HTTP server using the 'ClientEnv'
-}
runKupoUtxoQueryT :: ClientEnv -> KupoUtxoQueryT m a -> m a
runKupoUtxoQueryT env = flip runReaderT env . unKupoUtxoQueryT

-- utxosByPayment :: PaymentCredential -> m (UTxO BabbageEra)

data KupoConfig =
  KupoConfig
    { kcHost :: String
    , kcPort :: Int
    } deriving stock (Eq, Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

parseKupoConfig :: Parser KupoConfig
parseKupoConfig =
  KupoConfig
    <$> strOption   (long "kupo.host" <> help "Hostname of the kupo instance" <> Options.Applicative.value "localhost")
    <*> option auto (long "kupo.port" <> help "Port of the kupo instance" <> Options.Applicative.value 8081)

kupoClientEnv :: HTTP.Manager -> KupoConfig -> ClientEnv
kupoClientEnv manager KupoConfig{kcHost, kcPort} =
  mkClientEnv manager (BaseUrl Http kcHost kcPort "")
