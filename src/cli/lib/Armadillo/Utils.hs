module Armadillo.Utils (
  writeJSONFile,
  readJSONFile,
  readAssetId,
  unReadAssetId
) where

import           Cardano.Api              (AssetId)
import qualified Cardano.Api              as C
import           Control.Exception        (SomeException, catch)
import           Data.Aeson               (FromJSON, ToJSON, eitherDecode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor           (Bifunctor (..))
import qualified Data.ByteString.Lazy     as BSL
import           Data.Proxy               (Proxy (..))
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text

{-| Encode a value as JSON and write it to a file
-}
writeJSONFile :: ToJSON a => FilePath -> a -> IO ()
writeJSONFile file = BSL.writeFile file . encodePretty

{-| Decode a JSON value from a file
-}
readJSONFile :: FromJSON a => FilePath -> IO (Either String a)
readJSONFile fp =
  catch (eitherDecode <$> BSL.readFile fp) $ \(ex :: SomeException) -> pure (Left (show ex))

readAssetId :: Char -> String -> Either String AssetId
readAssetId separator = \case
  "ada" -> Right C.AdaAssetId
  x -> case splitAt 56 x of
        (policyId, sep : assetName) | sep == separator ->
          C.AssetId
            <$> (first (\err -> "Failed to decode policy ID: " <> show err) $ C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy) (Text.encodeUtf8 $ Text.pack policyId))
            <*> (first (\err -> "Failed to decode asset name: " <> show err) $ C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy) (Text.encodeUtf8 $ Text.pack assetName))
        _ -> Left $ "Unable to decode asset ID. Expected <56-character hex encoded policy ID>" ++ separator : "<hex-encoded asset name>"

unReadAssetId :: Char -> AssetId -> String
unReadAssetId separator = \case
  C.AdaAssetId -> "ada"
  C.AssetId policyId assetName ->
    Text.unpack $
      C.serialiseToRawBytesHexText policyId <> Text.pack [separator] <> C.serialiseToRawBytesHexText assetName
