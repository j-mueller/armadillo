{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
module Armadillo.Test.UnitTest(
  tests
) where

import           Armadillo.BuildTx              (PoolLiquidityToken (..),
                                                 PoolNFT (..), PoolOutput (..))
import qualified Armadillo.BuildTx              as BuildTx
import           Armadillo.Command              (CreatePoolParams (..),
                                                 createPool)
import           Armadillo.Scripts              (loadScriptsConfig,
                                                 scriptsFromScriptsConfig)
import qualified Armadillo.Test.Scripts         as Scripts
import           Cardano.Api                    (AssetName, PolicyId, TxIn,
                                                 Value)
import qualified Cardano.Api                    as C
import           Control.Lens                   (view)
import qualified Control.Lens                   as L
import           Control.Monad.Except           (ExceptT, runExceptT)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Convex.BuildTx                 (execBuildTx', payToAddress,
                                                 runBuildTxT,
                                                 setMinAdaDepositAll)
import           Convex.Class                   (MonadBlockchain (..),
                                                 MonadMockchain)
import           Convex.Lenses                  (emptyTx)
import qualified Convex.Lenses                  as L
import           Convex.MockChain.CoinSelection (balanceAndSubmit)
import qualified Convex.MockChain.Defaults      as Defaults
import           Convex.MockChain.Utils         (mockchainSucceeds)
import           Convex.Query                   (MonadUtxoQuery (..),
                                                 balanceAndSubmitOperator,
                                                 selectOperatorUTxO)
import           Convex.Utils                   (txnUtxos)
import           Convex.Wallet                  (Wallet)
import qualified Convex.Wallet.MockWallet       as Wallet
import           Convex.Wallet.Operator         (Operator (..),
                                                 PaymentExtendedKey (..),
                                                 Signing, verificationKey)
import           Data.Aeson                     (Result (..), fromJSON, object,
                                                 (.=))
import           Data.Bifunctor                 (Bifunctor (..))
import           Data.Function                  ((&))
import           Data.Proxy                     (Proxy (..))
import           Test.Tasty                     (TestTree, testGroup)
import           Test.Tasty.HUnit               (assertEqual, testCase)

tests :: TestTree
tests = testGroup "emulator"
  [ testCase "create LQ pool NFT" (mockchainSucceeds createLQPoolNft)
  , testCase "create LQ pool liquidity" (mockchainSucceeds createLQPoolLiquidity)
  , testCase "create LQ pool" (mockchainSucceeds createLQPool)
  ]

createLQPoolNft :: (MonadIO m, MonadMockchain m, MonadUtxoQuery m, MonadFail m) => m (C.Tx C.BabbageEra, (PolicyId, AssetName))
createLQPoolNft = do
  _ <- payToOperator Wallet.w2 testOperator
  _ <- payToOperator Wallet.w2 testOperator
  utxo <- selectOperatorUTxO testOperator >>= maybe (fail "No UTxO found") pure
  (PoolNFT{pnftAsset}, buildTx) <- failOnError (runBuildTxT (BuildTx.createPoolNft (fst utxo)))
  tx <- runExceptT (balanceAndSubmitOperator testOperator Nothing (buildTx emptyTx)) >>= either (fail . show) pure
  let (_, txOut) = head $ txnUtxos tx
  let nftAssetId = uncurry C.AssetId pnftAsset
      vl = view (L._TxOut . L._2 . L._TxOutValue . L._Value . L.at nftAssetId) txOut
  liftIO $ assertEqual "Should have pool NFT" (Just 1) vl
  pure $ (tx, pnftAsset)

createLQPoolLiquidity :: (MonadMockchain m, MonadUtxoQuery m, MonadFail m) => m (C.Tx C.BabbageEra, (PolicyId, AssetName))
createLQPoolLiquidity = do
  _ <- payToOperator Wallet.w2 testOperator
  _ <- payToOperator Wallet.w2 testOperator
  utxo <- selectOperatorUTxO testOperator >>= maybe (fail "No UTxO found") pure
  (PoolLiquidityToken{pltAsset}, buildTx) <- failOnError (runBuildTxT (BuildTx.createPoolLiquidityToken (fst utxo) 10_000))
  runExceptT (balanceAndSubmitOperator testOperator Nothing (buildTx emptyTx)) >>= either (fail . show) (pure . (,pltAsset))

createLQPool :: (MonadIO m, MonadUtxoQuery m, MonadMockchain m, MonadFail m) => m (PoolOutput TxIn)
createLQPool = failOnError $ do
  scripts <- liftIO loadScriptsConfig >>= liftIO . scriptsFromScriptsConfig >>= either (fail . (<>) "Failed to load scripts: " . show) pure
  _ <- payToOperator Wallet.w2 testOperator
  _ <- payToOperator Wallet.w2 testOperator
  pair1 <- Scripts.mintNativeToken Wallet.w2 "x" 1200
  pair2 <- Scripts.mintNativeToken Wallet.w2 "y" 1200
  _ <- payToOperator' (C.valueFromList [pair1, pair2]) Wallet.w2 testOperator
  let cpps = CreatePoolParams
              { cppOperator = testOperator
              , cppFee = 10
              , cppAssetClassX = second (\q -> q - 200) pair1
              , cppAssetClassY = second (\q -> q - 200) pair2
              }
  po@PoolOutput{poConfig} <- createPool scripts cpps
  let nftAssetId = BuildTx.poolNftAssetId poConfig
      vl = BuildTx.poolValue po
  liftIO $ assertEqual "Should have pool NFT" 1 (C.selectAsset vl nftAssetId)
  pure po

{-| Pay 100 Ada from one wallet to another
-}
payToOperator :: (MonadMockchain m, MonadFail m) => Wallet -> Operator k -> m (C.Tx C.BabbageEra)
payToOperator wFrom = payToOperator' (C.lovelaceToValue 100_000_000) wFrom

{-| Pay 100 Ada from one wallet to another
-}
payToOperator' :: (MonadMockchain m, MonadFail m) => Value -> Wallet -> Operator k -> m (C.Tx C.BabbageEra)
payToOperator' value wFrom Operator{oPaymentKey} = do
  p <- queryProtocolParameters
  let addr =
        C.makeShelleyAddressInEra Defaults.networkId
        (C.PaymentCredentialByKey $ C.verificationKeyHash $ verificationKey oPaymentKey)
        C.NoStakeAddress
      tx = execBuildTx' $ payToAddress addr value >> setMinAdaDepositAll p
  balanceAndSubmit wFrom tx

testOperator :: Operator Signing
testOperator =
  let oPaymentKey =
        either (error . (<>) "expected key. " . show) PESigningEx
        $ signingKeyFromCbor "5880b808c3a5df79e6b9130d15f11fbdc019246448250adb2fec1c2d0aaf0cca154aeee499350c8767aeed3d7043b9719a112f5a1cc594c2debcbc7ec779eb8b89ee049dbb91c5b3804f45551566c72a544758eea407c19f944de6b4c8b33678d1e8bda691ec03966b364650bf78ebb1a7d9011380cdcaf70446bf844dbe423dcd50"
  in Operator
      { oPaymentKey
      , oStakeKey = Nothing
      }

signingKeyFromCbor :: String -> Either String (C.SigningKey C.PaymentExtendedKey)
signingKeyFromCbor cbor = do
  let s :: String -> String
      s = id
      vl = object ["type" .= s "PaymentExtendedSigningKeyShelley_ed25519_bip32", "description" .= s "", "cborHex" .= cbor]
  textEnvelope <- fromJSON vl & (\case { Error err -> Left (show err); Success e -> Right e })
  C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy) textEnvelope & first show

failOnError :: (Show e, MonadFail m) => ExceptT e m a -> m a
failOnError x = runExceptT x >>= either (fail . show) pure
