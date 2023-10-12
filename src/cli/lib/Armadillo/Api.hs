{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Armadillo.Api(
  FullAPI,
  API,
  openAPI,
  writeApiToFile,
  AssetID(..),
  toCardanoAssetId,
  Pair(..),
  canonicalOrder,
  mkPair,
  PairID(..),
  getPairId,
  Statistic(..),
  Transaction(..),
  Direction(..),
  BuySell(..),
  AssetListEntry(..),
  UserAssetListEntry(..),
  UserLiquidity(..),
  UserID(..),
  PairTimeseriesPoint(..),
  DexTimeseriesPoint(..),
  HistoricPairData(..),
  FarmEntry(..),
  UserFarmEntry(..),
  FarmAssetData(..),
  UserFarmAssetData(..),
  LBEArgs(..),
  LBEResponse(..),
  TxHistoryEntry(..),
  -- ** API Parts
  HistoricAPI,
  LiquidityAPI,
  FarmAPI,
  LBEAPI,
  TxHistoryAPI,
  Healthcheck,
  InternalAPI,
  BuildTxAPI,

  -- * endpoints
  getHealth,
  getPairs,
  getTransactions,

  CreatePoolArgs(..),
  WrappedTx(..),
  buildCreatePoolTx,

  -- ** Internal endpoints
  getPoolOutputs,
  getDepositOutputs,

) where

import           Armadillo.BuildTx               (DepositOutput, PoolOutput)
import           Armadillo.Utils                 (readAssetId)
import           Cardano.Api                     (TxIn)
import qualified Cardano.Api                     as C
import           Data.Aeson                      (FromJSON (..), FromJSONKey,
                                                  ToJSON (..), ToJSONKey)
import           Data.Bifunctor                  (Bifunctor (..))
import qualified Data.ByteString.Lazy            as BSL
import           Data.OpenApi                    (ToParamSchema, ToSchema)
import           Data.OpenApi.Internal           (OpenApi)
import           Data.OpenApi.Internal.Utils     (encodePretty)
import           Data.Proxy                      (Proxy (..))
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import qualified Data.Text.Encoding              as Text
import           GHC.Generics                    (Generic)
import           PlutusLedgerApi.V1.Crypto       (PubKeyHash (..))
import           Servant.API                     (Capture, Description,
                                                  FromHttpApiData, Get, JSON,
                                                  NoContent, PlainText, Post,
                                                  QueryParam, ReqBody,
                                                  ToHttpApiData, type (:>),
                                                  (:<|>) (..))
import           Servant.API.ContentTypes        (MimeRender (..),
                                                  MimeUnrender (..))
import           Servant.Client                  (ClientEnv, client, runClientM)
import           Servant.Client.Core.ClientError (ClientError)
import           Servant.OpenApi                 (toOpenApi)
import qualified Text.Hex                        as Hex

newtype AssetID = AssetID Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, ToSchema, ToParamSchema, ToHttpApiData, FromHttpApiData)

toCardanoAssetId :: AssetID -> Either String C.AssetId
toCardanoAssetId (AssetID t) = readAssetId ':' (Text.unpack t)

newtype PairID = PairID Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, ToParamSchema, ToSchema, ToHttpApiData, FromHttpApiData)

{-| User (wallet) ID
-}
newtype UserID = UserID Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, ToJSONKey, FromJSONKey, ToParamSchema, ToSchema, ToHttpApiData, FromHttpApiData)

data Pair =
  Pair
    { assetIdX :: AssetID
    , assetIdY :: AssetID
    , pairID   :: PairID
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

canonicalOrder :: AssetID -> AssetID -> (AssetID, AssetID)
canonicalOrder a b = (min a b, max a b)

getPairId :: AssetID -> AssetID -> PairID
getPairId a b =
  let (AssetID a', AssetID b') = canonicalOrder a b
  in PairID (a' <> "." <> b')

mkPair :: AssetID -> AssetID -> Pair
mkPair a b =
  let (assetIdX, assetIdY) = canonicalOrder a b
      pairID = getPairId a b
  in Pair{ assetIdX, assetIdY, pairID}

data Statistic =
  Statistic
    { statLow  :: Double
    , statHigh :: Double
    , statAvg  :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data HistoricPairData =
  HistoricPairData
    { price     :: Statistic
    , marketCap :: Statistic
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data Transaction =
  Transaction
    { transactionID     :: Text
    , transactionType   :: Maybe BuySell
    , transactionPrice  :: Double
    , transactionInput  :: Double
    , transactionOutput :: Double
    , transactionOwner  :: Text
    , transactionPair   :: Maybe PairID
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type Limit = QueryParam "limit" Integer

data Direction = Ascending | Descending
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema, ToHttpApiData, FromHttpApiData)

data BuySell = Buy | Sell
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema, ToParamSchema, ToHttpApiData, FromHttpApiData)

type DirectionParam = QueryParam "direction" Direction

type SearchParam = QueryParam "search" Text

type PairParam = Capture "pair" PairID

type AssetParam = Capture "asset" AssetID

type UserParam = Capture "user" UserID

data PairTimeseriesPoint =
  PairTimeseriesPoint
    { pricePoint  :: Statistic
    , volumePoint :: Statistic
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data DexTimeseriesPoint =
  DexTimeseriesPoint
    { dexVolume :: Statistic
    , dexTVL    :: Statistic
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type Timeseries = [(Text, PairTimeseriesPoint)]

type DexTimeseries = [(Text, DexTimeseriesPoint)]

data AssetListEntry =
  AssetListEntry
    { pair           :: PairID
    , pairVolume     :: Statistic
    , pairTVL        :: Statistic
    , pairLPFee      :: Double
    , pairVolatility :: Double
    , pairLiquditity :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data UserAssetListEntry =
  UserAssetListEntry
    { userLiquidity :: Double
    , userAsset     :: AssetListEntry
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data UserLiquidityEntry =
  UserLiquidityEntry
    { liquidityPair  :: PairID
    , liquidityValue :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data Token =
  Token
    { tokenName :: Text
    , tokenIcon :: Text
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data UserLiquidity =
  UserLiquidity
    { userAPR       :: Double
    , poolDiversion :: Double -- ?
    , liquidity     :: [UserLiquidityEntry]
    , earningsMonth :: Double
    , tokensEarned  :: [Token]
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

-- HISTORIC DATA
-- 1) Summarized historic data
-- 	- List all trading pairs, or just search one
-- 		- Price, TVL, Volume (over specific timeframe), % change (over specific timeframe), Market cap
-- 		- Query params:
-- 			- timeframe (1h, 1d, 7d, etc)
-- 2) All transactions by token pair
-- 	- Created time, Type (buy or sell), Price, Input, Output, Onwer
-- 	- Query params:
-- 		- buy only
-- 		- sell only
-- 		- number to return
-- 3) Price and Volume chart for a specific token pair
-- 4) Volume and TVL chart data for TeddySwap

type HistoricAPI =
  "pairs" :> Get '[JSON] [Pair]
  :<|> "pair" :> "historic" :> Capture "timeframe-hours" Integer :> PairParam :> Description "Historic data for that pair over a number of hours" :> Get '[JSON] HistoricPairData
  :<|> "pair" :> "transactions" :> "buy" :> Limit :> PairParam :> Description "BUY transactions for a pair" :> Get '[JSON] [Transaction]
  :<|> "pair" :> "transactions" :> "sell" :> Limit :> PairParam :> Description "SELL transactions for a pair" :> Get '[JSON] [Transaction]
  :<|> "pair" :> "transactions" :> Limit :> PairParam :> Description "All transactions for a pair" :> Get '[JSON] [Transaction]
  :<|> "chart" :> "pair" :> PairParam :> Description "The price chart for a pair" :> Get '[JSON] Timeseries
  :<|> "chart" :> "dex" :> Description "The chart for the entire exchange" :> Get '[JSON] DexTimeseries

type LiquidityAPI =
  "assets" :> DirectionParam :> SearchParam :> Description "List of assets" :> Get '[JSON] [AssetListEntry]
  :<|> "assets" :> AssetParam :> Description "Data for a specific asset" :> Get '[JSON] AssetListEntry
  :<|> "assets" :> AssetParam :> UserParam :> Description "Asset data incl. user specific data" :> Get '[JSON] UserAssetListEntry
  :<|> "liquidity" :> UserParam :> Description "The user's liquidity" :> Get '[JSON] UserLiquidity

-- LIQUIDITY
-- 1) List assets
-- 	- Trading pair, TVL, Volume, LP Fee
-- 	- Query params: filter (ASC, DESC), search (list specific ones based on string)
-- 2) Specific asset
-- 	- Price: 1 ADA = 10 TEDY
-- 	- Total liquidity: eg ADA: 26,000,000, TEDY: 134,000,000
-- 	- Your liquidity (query the user's)
-- 	- Volatility index (how volatile is it over time)
-- 	- Volume:TVL ratio
-- 3) user's liquidity
-- 	- APR this week
-- 	- Pool diversion this week
-- 	- Sum of all user's liquidity
-- 		eg:
-- 			[{ value: '$100', name: 'ADA/TEDY'}, { value: '$200', name: 'cUSD/TEDY' }]
-- 	- Earnings this month
-- 	- Tokens you are earning (List of the tokens, object with name and icon)
-- 4) Add liquidity
-- 5) Remove liquidity

data FarmEntry =
  FarmEntry
    { totalStaked    :: Double
    , dailyEmissions :: Double
    , apr            :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data UserFarmEntry =
  UserFarmEntry
    { farmUser  :: UserID
    , farmEntry :: FarmEntry
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data FarmAssetData =
  FarmAssetData
    { pendingDistributionTotal :: Double
    , alreadyHarvested         :: Double
    , farmAPR                  :: Double
    , farmTotalStaked          :: Double
    , emissionsPercent         :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data UserFarmAssetData =
  UserFarmAssetData
    { usersStake    :: Double
    , farmAssetData :: FarmAssetData
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)


type FarmAPI =
  "farm" :> DirectionParam :> SearchParam :> Description "Farming" :> Get '[JSON] [FarmEntry]
  :<|> "farm" :> UserParam :> DirectionParam :> SearchParam :> Description "Farming with user-specific data" :> Get '[JSON] [UserFarmEntry]
  :<|> "farm" :> AssetParam :> Description "Farm related data for a specific asset" :> Get '[JSON] FarmAssetData
  :<|> "farm" :> AssetParam :> UserParam :> Description "Farm related data for a specific asset and user" :> Get '[JSON] UserFarmAssetData

-- FARM
-- 1) List assets
-- 	- Tokal staked, % emissions, Daily emissions, Your stake (query user's wallet), APR (calculated)
-- 	- Query params: filter (ASC, DESC), search (list specific ones based on string)
-- 2) Specific asset
-- 	- Pending distribution total
-- 	- Harvested already
-- 	- Farm APR
-- 	- Total staked
-- 	- % of emissions
-- 	- User's staked
-- 3) Harvest (collect only rewards)
-- 4) Withdraw (remove some tokens)
-- 5) Stake (create tx)

data LBEArgs =
  LBEArgs
    { lbePair             :: Pair
    , lbeInitialLiqudityX :: Double
    , lbeInitialLiqudityY :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

data LBEResponse =
  LBEResponse
    { estimatedLPPool    :: Double
    , estimatedTEDYPrice :: Double
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type LBEAPI =
  "lbe" :> ReqBody '[JSON] LBEArgs :> Description "LBE (TBD)" :> Post '[JSON] LBEResponse

-- LBE
-- 1) Estimated LP pool
-- 2) Estimated TEDY price
-- 3) Create transaction (send funds from user to LBE wallet)
-- 4) Estimate how much TEDY-ADA token you get
-- 5) LBE Status (time remaining, end date, current pool)
-- 6) User's already purchased number of TEDY-ADA

data Action = Swap | Harvest | AddLiquidity | Withdraw
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Status = Complete | Pending
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TxHistoryEntry =
  TxHistoryEntry
    { asset  :: AssetID
    , action :: Action
    , date   :: Text
    , status :: Status
    , txId   :: Text
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

type TxHistoryAPI =
  "tx-history" :> UserParam :> Limit :> Description "History of the user's transactions" :> Get '[JSON] [TxHistoryEntry]

-- TRANSACTION HISTORY
-- 1) List all user's transactions
-- 	- Assets, Action (swap, harvest, add liquidity, etc), Date, Status (complete, pending), Transaction ID (so we can link ada explorer)
-- 2) Refund stuck tx (it moved past slippage)

type Healthcheck = "healthcheck" :> Description "Is the server alive?" :> Get '[JSON] NoContent

type FullAPI =
  API
  :<|> ("internal" :> InternalAPI)
  :<|> ("build-tx" :> BuildTxAPI)

type API =
  Healthcheck
  :<|> HistoricAPI
  :<|> LiquidityAPI
  :<|> FarmAPI
  :<|> LBEAPI
  :<|> TxHistoryAPI

-- TRADE PANEL
-- 1) Market swap
-- 	- create transaction
-- 	- refund transaction

openAPI :: OpenApi
openAPI = toOpenApi (Proxy @API)

openAPIBS :: BSL.ByteString
openAPIBS = encodePretty openAPI

writeApiToFile :: FilePath -> IO ()
writeApiToFile fp = BSL.writeFile fp openAPIBS

-- * Calling API endpoints

{-| Call the "healthcheck" endpoint
-}
getHealth :: ClientEnv -> IO (Either ClientError NoContent)
getHealth clientEnv = do
  let healthcheck :<|> _ = client (Proxy @API)
  runClientM healthcheck clientEnv

getPairs :: ClientEnv -> IO (Either ClientError [Pair])
getPairs clientEnv = do
  let _healthcheck :<|> (pairs :<|> _) :<|> _ = client (Proxy @API)
  runClientM pairs clientEnv

getTransactions :: ClientEnv -> Maybe Integer -> PairID -> IO (Either ClientError [Transaction])
getTransactions clientEnv limit pair = do
  let _healthcheck :<|> (_pairs :<|> _historic :<|> _buyTxns :<|> _sellTxns :<|> allTxns :<|> _) :<|> _ = client (Proxy @API)
  runClientM (allTxns limit pair) clientEnv

-- API for internal use (CLI)
type InternalAPI =
  "pools" :> Get '[JSON] [PoolOutput TxIn]
  :<|> "deposits" :> Get '[JSON] [DepositOutput TxIn]

getPoolOutputs :: ClientEnv -> IO (Either ClientError [PoolOutput TxIn])
getPoolOutputs =
  let pools :<|> _ = client (Proxy @("internal" :> InternalAPI))
  in runClientM pools

getDepositOutputs :: ClientEnv -> IO (Either ClientError [DepositOutput TxIn])
getDepositOutputs =
  let _pools :<|> deposits = client (Proxy @("internal" :> InternalAPI))
  in runClientM deposits

newtype WrappedTx = WrappedTx (C.Tx C.BabbageEra)

instance ToJSON WrappedTx where
  toJSON (WrappedTx tx) = toJSON (C.serialiseToTextEnvelope Nothing tx)

instance FromJSON WrappedTx where
  parseJSON x = parseJSON x >>= either (fail . show) (pure . WrappedTx) . C.deserialiseFromTextEnvelope (C.proxyToAsType Proxy)

instance MimeRender PlainText WrappedTx where
  mimeRender _ (WrappedTx tx) = BSL.fromStrict $ Text.encodeUtf8 $ Hex.encodeHex $ C.serialiseToCBOR tx

instance MimeUnrender PlainText WrappedTx where
  mimeUnrender _ bs = do
    txt <- first show (Text.decodeUtf8' $ BSL.toStrict bs)
    binary <- maybe (Left "Bad hex format") pure (Hex.decodeHex txt)
    bimap (\err -> "Deserialisation from CBOR failed: " <> show err) WrappedTx (C.deserialiseFromCBOR (C.proxyToAsType Proxy) binary)

data CreatePoolArgs =
  CreatePoolArgs
    { cpoAssetX            :: AssetID
    , cpoAssetY            :: AssetID
    , cpoQuantityX         :: Integer
    , cpoQuantityY         :: Integer
    , cpoFeeNumerator      :: Integer
    , cpoPublicKeyHash     :: PubKeyHash
    , cpoStakingCredential :: Maybe PubKeyHash
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type BuildTxAPI =
  "create-pool" :> Description "Create a new AMM pool" :> ReqBody '[JSON] CreatePoolArgs :> Post '[JSON] WrappedTx

buildCreatePoolTx :: ClientEnv -> CreatePoolArgs -> IO (Either ClientError WrappedTx)
buildCreatePoolTx clientEnv args =
  let postTx = client (Proxy @("build-tx" :> BuildTxAPI))
  in runClientM (postTx args) clientEnv
