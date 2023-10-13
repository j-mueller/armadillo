{-# LANGUAGE OverloadedStrings #-}
module Armadillo.Server.Mock(
  mockHistoricApi,
  mockLiquidityAPI,
  mockFarmAPI,
  mockLBEAPI,
  mockTxHistoryAPI,
  mockInternalAPI,
  mockBuildTxAPI,

  -- * Values
  djedAdaPair,

  -- ** Individual routes
  getHistoric,
  getBuyTxns,
  getSellTxns,
  getAllTxns,
  getChartForPair,
  getChartForDex
  ) where

import           Armadillo.Api        (AssetID (..), AssetListEntry (..),
                                       BuildTxAPI, BuySell (..),
                                       DexTimeseriesPoint (..), Direction (..),
                                       FarmAPI, FarmAssetData (..),
                                       FarmEntry (..), HistoricAPI,
                                       HistoricPairData (..), InternalAPI,
                                       LBEAPI, LBEArgs (..), LBEResponse (..),
                                       LiquidityAPI, Pair (..), PairID (..),
                                       PairTimeseriesPoint (..), Statistic (..),
                                       Transaction (..), TxHistoryAPI,
                                       TxHistoryEntry, UserAssetListEntry (..),
                                       UserFarmAssetData (..),
                                       UserFarmEntry (..), UserID (..),
                                       UserLiquidity (..), getPairId, mkPair)
import           Control.Monad.Except (MonadError (throwError))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.String          (IsString (..))
import           Data.Text            (Text)
import           Servant.API          ((:<|>) (..))
import           Servant.Server       (Server, ServerError, err404, err501,
                                       errBody)

mockHistoricApi :: Server HistoricAPI
mockHistoricApi =
  getPairs
  :<|> getHistoric
  :<|> getBuyTxns
  :<|> getSellTxns
  :<|> getAllTxns
  :<|> getChartForPair
  :<|> getChartForDex

-- * Mock asset IDs
djedAssetId :: AssetID
djedAssetId = AssetID "asset15f3ymkjafxxeunv5gtdl54g5qs8ty9k84tq94x"

shenAssetId :: AssetID
shenAssetId = AssetID "asset17v9z2sf7v05z6mne4qk0kzlmue5aqxtfxq8jyk"

adaAssetId :: AssetID
adaAssetId = AssetID "ada"

pairs :: [(AssetID, AssetID)]
pairs = [(djedAssetId, adaAssetId), (shenAssetId, adaAssetId)]

djedAdaPair :: Pair
djedAdaPair = mkPair djedAssetId adaAssetId

getPairs :: Monad m => m [Pair]
getPairs = pure $ uncurry mkPair <$> pairs

pairData :: Map PairID HistoricPairData
pairData =
  Map.fromList
    [ ( getPairId djedAssetId adaAssetId
      , HistoricPairData
          { price     = Statistic{statLow = 3.98,       statHigh = 4.10, statAvg = 4.02}
          , marketCap = Statistic{statLow = 3745821.42, statHigh = 3750821.42, statAvg = 3749821.42}
          }
      )
    , ( getPairId shenAssetId adaAssetId
      , HistoricPairData
          { price     = Statistic{statLow = 1.05,     statHigh = 1.12, statAvg = 1.09 }
          , marketCap = Statistic{statLow = 31456010, statHigh = 33452010, statAvg = 32456010}
          })
    ]

getHistoric :: MonadError ServerError m => Integer -> PairID -> m HistoricPairData
getHistoric _ p =
  let err = throwError $ err404{ errBody = "Unknown pair: " <> fromString (show p) }
  in maybe err pure $ Map.lookup p pairData

txn1 :: Transaction
txn1 =
  Transaction
    { transactionID = "970092f3e476d9f2cfdebedae17074601a520a9632b9030cfd1a3c79d714679c"
    , transactionType = Just Buy
    , transactionPrice = 1.13
    , transactionInput = 12.8
    , transactionOutput = 11.67
    , transactionOwner = "stake1u8uvqhg9m603kdm247mzhsw6g7whauwr9tc7wvrstcrnxj9jjsg6p"
    , transactionPair = Just (pairID djedAdaPair)
    }

txn2 :: Transaction
txn2 =
  Transaction
    { transactionID = "92f3e476d9f2cfde9d1a3c79d714679c700bedae17074601a520a9632b9030cf"
    , transactionType = Just Sell
    , transactionPrice = 1.13
    , transactionInput = 12.8
    , transactionOutput = 11.67
    , transactionOwner = "stake1u8uvqhg9m603kdm247mzhsw6g7whauwr9tc7wvrstcrnxj9jjsg6p"
    , transactionPair = Just (pairID djedAdaPair)
    }

txn3 :: Transaction
txn3 =
  Transaction
    { transactionID = "92f3e476d9f2cfde9d1a3c79d714679c700bedae17074601a520a9632b9030cf"
    , transactionType= Just Sell
    , transactionPrice = 1.13
    , transactionInput = 12.8
    , transactionOutput = 11.67
    , transactionOwner = "stake1u8uvqhg9m603kdm247mzhsw6g7whauwr9tc7wvrstcrnxj9jjsg6p"
    , transactionPair = Just (pairID djedAdaPair)
    }

allTxns :: [Transaction]
allTxns = [txn1, txn2, txn3]

getBuyTxns :: Monad m => Maybe Integer -> PairID -> m [Transaction]
getBuyTxns _ _ = pure $ filter ((==) (Just Buy) . transactionType) allTxns

getSellTxns :: Monad m => Maybe Integer -> PairID -> m [Transaction]
getSellTxns _ _ = pure $ filter ((==) (Just Buy) . transactionType) allTxns

getAllTxns :: Monad m => Maybe Integer -> PairID -> m [Transaction]
getAllTxns _ p = pure $ filter ((==) (Just p) . transactionPair) allTxns

getChartForPair :: Monad m => PairID -> m [(Text, PairTimeseriesPoint)]
getChartForPair _ = pure []

getChartForDex :: Monad m => m [(Text, DexTimeseriesPoint)]
getChartForDex = pure []

mockLiquidityAPI :: Server LiquidityAPI
mockLiquidityAPI =
  getAssets
  :<|> getAsset
  :<|> getUserAssets
  :<|> getLiquidity

getAssets :: Monad m => Maybe Direction -> Maybe Text -> m [AssetListEntry]
getAssets _ _ = pure []

getAsset :: Monad m => AssetID -> m AssetListEntry
getAsset _ = pure undefined

getUserAssets :: AssetID -> UserID -> m UserAssetListEntry
getUserAssets _ _ = undefined

getLiquidity :: UserID -> m UserLiquidity
getLiquidity _ = undefined

mockFarmAPI :: Server FarmAPI
mockFarmAPI =
  getFarmEntries
  :<|> getUserFarmEntries
  :<|> getFarmAssetData
  :<|> getUserFarmAssetData

getFarmEntries :: Monad m => Maybe Direction -> Maybe Text -> m [FarmEntry]
getFarmEntries _ _ = pure []

getUserFarmEntries :: Monad m => UserID -> Maybe Direction -> Maybe Text -> m [UserFarmEntry]
getUserFarmEntries _ _ _ = pure []

getFarmAssetData :: Monad m => AssetID -> m FarmAssetData
getFarmAssetData _ = pure undefined

getUserFarmAssetData :: AssetID -> UserID -> m UserFarmAssetData
getUserFarmAssetData _ _ = undefined

mockLBEAPI :: Server LBEAPI
mockLBEAPI = getLBE

getLBE :: LBEArgs -> m LBEResponse
getLBE _ = undefined

mockTxHistoryAPI :: Server TxHistoryAPI
mockTxHistoryAPI = getTxHistory

getTxHistory :: Monad m => UserID -> Maybe Integer -> m [TxHistoryEntry]
getTxHistory _ _ = pure []

mockInternalAPI :: Server InternalAPI
mockInternalAPI = pure [] :<|> pure []

mockBuildTxAPI :: Server BuildTxAPI
mockBuildTxAPI =
    const (throwError err501)
    :<|> const (throwError err501)
    :<|> const (throwError err501)
    :<|> const (throwError err501)
