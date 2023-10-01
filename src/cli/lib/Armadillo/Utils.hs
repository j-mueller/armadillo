module Armadillo.Utils (
  writeJSONFile,
  readJSONFile
) where

import           Control.Exception        (SomeException, catch)
import           Data.Aeson               (FromJSON, ToJSON, decode)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as BSL

{-| Encode a value as JSON and write it to a file
-}
writeJSONFile :: ToJSON a => FilePath -> a -> IO ()
writeJSONFile file = BSL.writeFile file . encodePretty

{-| Decode a JSON value from a file
-}
readJSONFile :: FromJSON a => FilePath -> IO (Maybe a)
readJSONFile fp =
  catch (decode <$> BSL.readFile fp) $ \(_ :: SomeException) -> pure Nothing
