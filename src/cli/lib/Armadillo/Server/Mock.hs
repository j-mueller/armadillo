{-# LANGUAGE OverloadedStrings #-}
module Armadillo.Server.Mock(
  mockHistoricApi,
  mockLiquidityAPI,
  mockFarmAPI,
  mockLBEAPI,
  mockTxHistoryAPI,

  -- * Values
  ) where

import           Armadillo.Api        (AssetID (..), AssetListEntry (..),
                                       DexTimeseriesPoint (..), Direction (..),
                                       FarmAPI, FarmAssetData (..),
                                       FarmEntry (..), HistoricAPI,
                                       HistoricPairData (..), LBEAPI,
                                       LBEArgs (..), LBEResponse (..),
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
import           Servant.Server       (Server, ServerError, err404, errBody)

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

getBuyTxns :: Monad m => Maybe Integer -> PairID -> m [Transaction]
getBuyTxns _ _ = pure []

getSellTxns :: Monad m => Maybe Integer -> PairID -> m [Transaction]
getSellTxns _ _ = pure []

getAllTxns :: Monad m => Maybe Integer -> PairID -> m [Transaction]
getAllTxns _ _ = pure []

getChartForPair :: Monad m => PairID -> m [(Text, PairTimeseriesPoint)]
getChartForPair _ = pure []

getChartForDex :: Monad m => m [(Text, DexTimeseriesPoint)]
getChartForDex = pure []

zeroStat :: Statistic
zeroStat = Statistic 0 0 0

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

getUserFarmAssetData :: Monad m => AssetID -> UserID -> m UserFarmAssetData
getUserFarmAssetData _ _ = undefined

mockLBEAPI :: Server LBEAPI
mockLBEAPI = getLBE

getLBE :: LBEArgs -> m LBEResponse
getLBE _ = undefined

mockTxHistoryAPI :: Server TxHistoryAPI
mockTxHistoryAPI = getTxHistory

getTxHistory :: Monad m => UserID -> Maybe Integer -> m [TxHistoryEntry]
getTxHistory _ _ = pure []
