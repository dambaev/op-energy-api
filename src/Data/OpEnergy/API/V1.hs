{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DuplicateRecordFields      #-}
module Data.OpEnergy.API.V1 where

import           Data.Swagger
import           Control.Lens
import           GHC.Generics
import           Data.Typeable              (Typeable)
import           Data.Aeson
import           Servant.API
import           Data.Scientific( Scientific, toBoundedInteger)
import qualified Data.Text.Read as TR
import           Data.Text                  (Text)


-- | API specifications of a backend service for Swagger
type V1API
  = "swagger.json"
    :> Description "Returns Swagger specification in JSON format"
    :> Get '[JSON] Text

  :<|> "register"
    :> Description "Registers new user and returns randomly generated account secret and account token.\n Account secret should be used for /login API encpoint.\n Account token should be used in the rest API calls as an authentication cookie"
    :> Post '[JSON] RegisterResult

  :<|> "login"
    :> ReqBody '[JSON] [AccountSecret]
    :> Description "Performs login with given account secret. Returns AccountToken value for being used with the rest API calls"
    :> Post '[JSON] [AccountToken]

  :<|> "strike"
    :> "mediantime"
    :> Description "Returns list of strikes created by mediantime"
    :> Get '[JSON] [TimeStrike]

  :<|> "strike"
    :> "block"
    :> "mediantime"
    :> Description "Returns list of strikes by a given block height"
    :> QueryParam "block_height" BlockHeight
    :> Get '[JSON] [TimeStrike]

  :<|> "strike"
    :> "mediantime"
    :> Description "Creates new strike defined by BlockHeight and NLockTime values"
    :> ReqBody '[JSON] CreateTimeStrikeRequest
    :> Post '[JSON] TimeStrike

  :<|> "slowfastguess"
    :> "mediantime"
    :> QueryParam "block_height" BlockHeight
    :> QueryParam "nlocktime" NLockTime
    :> Description ""
    :> Get '[JSON] [SlowFastGuess]

  :<|> "slowfastguess"
    :> "mediantime"
    :> ReqBody '[JSON] CreateSlowFastGuessRequest
    :> Description "Creates a slow/fast guess for a given time strike"
    :> Post '[JSON] SlowFastGuess

  :<|> "strikeshistory"
    :> "mediantime"
    :> Description "Returns list of archived time strikes  (ie, already fired and moved into an archive)"
    :> Get '[JSON] [TimeStrikeHistory]

  :<|> "slowfastresults"
    :> "mediantime"
    :> QueryParam "account_token" AccountToken
    :> QueryParam "nlocktime" NLockTime
    :> QueryParam "block_height" BlockHeight
    :> Get '[JSON] [SlowFastResult]

  :<|> "user"
    :> "displayname"
    :> ReqBody '[JSON] PostUserDisplayNameRequest
    :> Description "Updates displayname for a given user"
    :> Post '[JSON] ()

  :<|> "statistics"
    :> Capture "blockheight" Text
    :> Capture "span" Text
    :> Description "Calculates NBDR statistics for a given block height and span"
    :> Get '[JSON] Statistics

  :<|> "oe"
    :> "block"
    :> Capture "hash" BlockHash
    :> Description "Returns block's header by a given block hash, including chainwork, that is missing from mempool's blocks' headers cache"
    :> Get '[JSON] Block

  :<|> "oe"
    :> "blockspanlist"
    :> Capture "startBlockHeight" (Natural Int)
    :> Capture "span" (Positive Int)
    :> Capture "numberOfSpan" (Positive Int)
    :> Description "Returns list of spans started from startBlockHeight of size span and numberOfSpan length "
    :> Get '[JSON] [BlockSpan]

data Block = Block
  { id:: Text
  , height:: Int
  , version:: Int
  , timestamp:: Int
  , bits:: Int
  , nonce:: Int
  , difficulty:: Double
  , merkle_root:: Text
  , tx_count:: Int
  , size:: Int
  , weight:: Int
  , previousblockhash :: BlockHash
  , chainwork:: Text
  , mediantime:: Int
  }
  deriving (Show, Generic, Typeable)

defaultBlock:: Block
defaultBlock = Block
  { id = "0000000000000000000135d442ddb5ad7a8cdf92eb8496265d724804587bdf41"
  , height = 772473
  , version = 538304512
  , timestamp = 1674018057
  , bits = 386366690
  , nonce = 2589914493
  , difficulty = 37590453655497.09
  , merkle_root = "847457eb7723bbe1e60a73ad6ff3016b630bf3595409eaa6a3f45e3cc1b54cf0"
  , tx_count = 2303
  , size = 1528844
  , weight = 3992705
  , previousblockhash = BlockHash "00000000000000000004fd7d4e275888070a2c57fbbaa145d576f935f67645f8"
  , chainwork = "00000000000000000000000000000000000000003dfd08c2b6932fc194a1fee4"
  , mediantime = 1674012509
  }

instance ToJSON Block
instance FromJSON Block
instance ToSchema Block where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Block schema"
    & mapped.schema.example ?~ toJSON defaultBlock

newtype BlockHash = BlockHash Text
  deriving (Show, Generic, Typeable)

defaultBlockHash :: BlockHash
defaultBlockHash = BlockHash "000000000000000000070654ba6de216fc6b5d5c7279e5695b4225d7a6ed993a"

instance ToJSON BlockHash
instance FromJSON BlockHash
instance ToSchema BlockHash where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockHash schema"
    & mapped.schema.example ?~ toJSON defaultBlockHash
instance ToParamSchema BlockHash where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString

data BlockSpan = BlockSpan
  { startBlockHeight :: BlockHeight
  , endBlockHeight :: BlockHeight
  }
  deriving (Show, Generic, Typeable)

defaultBlockSpan :: BlockSpan
defaultBlockSpan = BlockSpan (BlockHeight 772472) (BlockHeight 772473)

instance ToJSON BlockSpan
instance FromJSON BlockSpan
instance ToSchema BlockSpan where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockSpan schema"
    & mapped.schema.example ?~ toJSON defaultBlockSpan


data NbdrStatistics = NbdrStatistics
  { avg :: Double
  , stddev :: Double
  }
  deriving (Show, Generic, Typeable)

defaultNbdrStatistics :: NbdrStatistics
defaultNbdrStatistics = NbdrStatistics
  { avg = 1
  , stddev = 1
  }

instance ToJSON NbdrStatistics
instance FromJSON NbdrStatistics
instance ToSchema NbdrStatistics where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "NbdrStatistics schema"
    & mapped.schema.example ?~ toJSON defaultNbdrStatistics

data Statistics = Statistics
  { nbdr :: NbdrStatistics
  }
  deriving (Show, Generic, Typeable)

defaultStatistics :: Statistics
defaultStatistics = Statistics
  { nbdr = defaultNbdrStatistics
  }

instance ToJSON Statistics
instance FromJSON Statistics
instance ToSchema Statistics where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "Statistics schema"
    & mapped.schema.example ?~ toJSON defaultStatistics


data SlowFastResult = SlowFastResult
  { guess :: Guess
  , result :: GuessResult
  , blockHeight :: BlockHeight
  , nLockTime :: NLockTime
  , creationTime :: Positive Int
  }
  deriving (Show, Generic, Typeable)

defaultSlowFastResult :: SlowFastResult
defaultSlowFastResult = SlowFastResult
  { guess = Slow
  , result = GuessResultRight
  , blockHeight = defaultBlockHeight
  , nLockTime = defaultNLockTime
  , creationTime = verifyPositive 1
  }

instance ToJSON SlowFastResult
instance FromJSON SlowFastResult
instance ToSchema SlowFastResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SlowFastResult schema"
    & mapped.schema.example ?~ toJSON defaultSlowFastResult

data RegisterResult = RegisterResult
  { accountSecret :: AccountSecret
  , accountToken  :: AccountToken
  }
  deriving (Show, Generic, Typeable)

defaultRegisterResult :: RegisterResult
defaultRegisterResult = RegisterResult defaultAccountSecret defaultAccountToken

instance ToJSON RegisterResult
instance FromJSON RegisterResult
instance ToSchema RegisterResult where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "RegisterResult schema"
    & mapped.schema.example ?~ toJSON defaultRegisterResult


data TimeStrikeHistory = TimeStrikeHistory
  { owner :: Text
  , blockHeight  :: BlockHeight
  , nLockTime    :: NLockTime
  , mediantime   :: Positive Int
  , creationTime :: Positive Int
  , archiveTime  :: Positive Int
  , wrongResults :: Natural Int
  , rightResults :: Natural Int
  }
  deriving (Show, Generic, Typeable)
instance ToJSON TimeStrikeHistory
instance FromJSON TimeStrikeHistory
instance ToSchema TimeStrikeHistory where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "TimeStrikeHistory schema"
    & mapped.schema.example ?~ toJSON defaultTimeStrikeHistory

defaultTimeStrikeHistory :: TimeStrikeHistory
defaultTimeStrikeHistory = TimeStrikeHistory
  { owner = "userName"
  , blockHeight = defaultBlockHeight
  , nLockTime = defaultNLockTime
  , mediantime = verifyPositiveInt 1000
  , creationTime = verifyPositiveInt 1000
  , archiveTime = verifyPositive 1001
  , wrongResults = verifyNaturalInt 0
  , rightResults = verifyNaturalInt 0
  }


data GuessResult = GuessResultRight | GuessResultWrong
  deriving (Show, Generic, Typeable, Enum)
defaultResult :: GuessResult
defaultResult = GuessResultWrong

instance Bounded GuessResult where
  minBound = GuessResultRight
  maxBound = GuessResultWrong
instance ToJSON GuessResult where
  toJSON GuessResultRight = toJSON ("right":: Text)
  toJSON GuessResultWrong = toJSON ("wrong":: Text)
instance FromJSON GuessResult where
  parseJSON = withText "Result" $ \v-> return $
    case v of
      "right" -> GuessResultRight
      "wrong" -> GuessResultWrong
      _ -> error "FromJSON GuessResult: unimplemented"
instance ToSchema GuessResult where
  declareNamedSchema _ = return $ NamedSchema (Just "Result") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom GuessResultRight)

data Guess = Slow | Fast
  deriving (Show, Generic, Typeable, Enum)
defaultGuess :: Guess
defaultGuess = Fast

instance Bounded Guess where
  minBound = Slow
  maxBound = Fast
instance ToJSON Guess where
  toJSON Slow = toJSON ("slow":: Text)
  toJSON Fast = toJSON ("fast":: Text)
instance FromJSON Guess where
  parseJSON = withText "Guess" $ \v-> return $
    case v of
      "slow" -> Slow
      "fast" -> Fast
      _ -> error "FromJSON Guess: unimplemented"
instance ToSchema Guess where
  declareNamedSchema _ = return $ NamedSchema (Just "Guess") $ mempty
    & type_ ?~ SwaggerString
    & enum_ ?~ (map toJSON $ enumFrom Slow)

data PostUserDisplayNameRequest = PostUserDisplayNameRequest
  { account_token :: AccountToken
  , display_name :: Text
  }
  deriving (Show, Generic, Typeable)
defaultPostUserDisplayNameRequest :: PostUserDisplayNameRequest
defaultPostUserDisplayNameRequest = PostUserDisplayNameRequest
  { account_token = defaultAccountToken
  , display_name = "newUserName"
  }
instance ToJSON PostUserDisplayNameRequest
instance FromJSON PostUserDisplayNameRequest
instance ToSchema PostUserDisplayNameRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "PostUserDisplayNameRequest schema"
    & mapped.schema.example ?~ toJSON defaultPostUserDisplayNameRequest

data CreateSlowFastGuessRequest = CreateSlowFastGuessRequest
  { account_token :: AccountToken
  , guess :: Guess
  , nlocktime :: NLockTime
  , block_height :: BlockHeight
  }
  deriving (Show, Generic, Typeable)

defaultCreateSlowFastGuessRequest :: CreateSlowFastGuessRequest
defaultCreateSlowFastGuessRequest = CreateSlowFastGuessRequest
  { account_token = defaultAccountToken
  , guess = defaultGuess
  , nlocktime = defaultNLockTime
  , block_height = defaultBlockHeight
  }
instance ToJSON CreateSlowFastGuessRequest
instance FromJSON CreateSlowFastGuessRequest
instance ToSchema CreateSlowFastGuessRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "CreateSlowFastGuessRequest schema"
    & mapped.schema.example ?~ toJSON defaultCreateSlowFastGuessRequest

data SlowFastGuess = SlowFastGuess
  { guess :: Guess
  , blockHeight :: BlockHeight
  , nLockTime :: NLockTime
  , creationTime :: Positive Int
  , userName :: Text
  , userId :: Natural Int
  }
  deriving (Show, Generic, Typeable)

defaultSlowFastGuess :: SlowFastGuess
defaultSlowFastGuess = SlowFastGuess
  { guess = Slow
  , blockHeight = defaultBlockHeight
  , nLockTime = defaultNLockTime
  , creationTime = verifyPositive 1
  , userName = "someUserName"
  , userId = verifyNaturalInt 0
  }

instance ToJSON SlowFastGuess
instance FromJSON SlowFastGuess
instance ToSchema SlowFastGuess where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "SlowFastGuess schema"
    & mapped.schema.example ?~ toJSON defaultSlowFastGuess

data CreateTimeStrikeRequest = CreateTimeStrikeRequest
  { account_token :: AccountToken
  , nlocktime :: NLockTime
  , block_height :: BlockHeight
  }
  deriving (Show, Generic, Typeable)

defaultCreateTimeStrikeRequest :: CreateTimeStrikeRequest
defaultCreateTimeStrikeRequest = CreateTimeStrikeRequest defaultAccountToken defaultNLockTime defaultBlockHeight

instance ToJSON CreateTimeStrikeRequest
instance FromJSON CreateTimeStrikeRequest
instance ToSchema CreateTimeStrikeRequest where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "CreateTimeStrikeRequest schema"
    & mapped.schema.example ?~ toJSON defaultCreateTimeStrikeRequest

newtype AccountToken = AccountToken Text
  deriving (Show, Generic, Typeable)
instance ToJSON AccountToken
instance FromJSON AccountToken
instance ToSchema AccountToken where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountToken schema"
    & mapped.schema.example ?~ toJSON defaultAccountToken
instance ToParamSchema AccountToken where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4ddb35fae6cedc9d84c86fd280157b7a93b984c0b40baf7f21b8f72"

defaultAccountToken :: AccountToken
defaultAccountToken = AccountToken "a86c139a32e7dac42afe4265a955a0fd9d8c2885e26c7e92d4270b3813faa356"

newtype AccountSecret = AccountSecret Text
  deriving (Show, Generic, Typeable)
instance ToJSON AccountSecret
instance FromJSON AccountSecret
instance ToSchema AccountSecret where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "AccountSecret schema"
    & mapped.schema.example ?~ toJSON defaultAccountSecret
instance ToParamSchema AccountSecret where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerString
    & format ?~ "b8ab3013e4ddb35fae6cedc9d84c86fd280157b7a93b984c0b40baf7f21b8f72"


defaultAccountSecret :: AccountSecret
defaultAccountSecret = AccountSecret "a86c139a32e7dac42afe4265a955a0fd9d8c2885e26c7e92d4270b3813faa356"


newtype BlockHeight = BlockHeight (Natural Int)
  deriving (Show, Typeable, Generic)

defaultBlockHeight :: BlockHeight
defaultBlockHeight = BlockHeight $ verifyNaturalInt 1
  
instance ToJSON BlockHeight
instance FromJSON BlockHeight
instance ToSchema BlockHeight where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "BlockHeight schema"
    & mapped.schema.example ?~ toJSON defaultBlockHeight
instance ToParamSchema BlockHeight where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 0

newtype NLockTime = NLockTime (Natural Int)
  deriving (Show, Typeable, Generic)

defaultNLockTime :: NLockTime
defaultNLockTime = NLockTime $ verifyNaturalInt 1
  
instance ToJSON NLockTime
instance FromJSON NLockTime
instance ToSchema NLockTime where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "NLockTime schema"
    & mapped.schema.example ?~ toJSON defaultNLockTime
instance ToParamSchema NLockTime where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 0


data TimeStrike = TimeStrike
  { blockHeight  :: BlockHeight
  , nLockTime    :: NLockTime
  , creationTime :: Positive Int
  }
  deriving (Show, Generic, Typeable)
instance ToJSON TimeStrike
instance FromJSON TimeStrike
instance ToSchema TimeStrike where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
    & mapped.schema.description ?~ "TimeStrike schema"
    & mapped.schema.example ?~ toJSON defaultTimeStrike

defaultTimeStrike :: TimeStrike
defaultTimeStrike = TimeStrike
  { blockHeight = defaultBlockHeight
  , nLockTime = defaultNLockTime
  , creationTime = 1000
  }


newtype Positive a = Positive Int
  deriving (Show, Generic, Typeable, Eq)

instance Num (Positive a) where
  (+) (Positive left) (Positive right) = verifyPositiveInt $! left + right
  (-) (Positive left) (Positive right) = verifyPositiveInt $! left - right
  (*) (Positive left) (Positive right) = verifyPositiveInt $! left * right
  abs v = v
  signum (Positive left) = verifyPositiveInt $ signum left
  fromInteger v = verifyPositiveInt (fromInteger v)

instance ToJSON (Positive Int)
instance FromJSON (Positive Int) where
  parseJSON = withScientific "Positive" $ \v-> return (verifyPositive v)
instance ToSchema (Positive Int) where
  declareNamedSchema _ = return $ NamedSchema (Just "Positive") $ mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 1
instance ToParamSchema (Positive Int) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 1

verifyPositiveInt:: Int -> Positive a
verifyPositiveInt v =
  if v > 0
  then Positive v
  else error "verifyPositiveInt: wrong value"

verifyPositive:: Scientific -> Positive a
verifyPositive s =
  case toBoundedInteger s of
    Just v -> verifyPositiveInt v
    _  -> error "verifyPositive: wrong value"


newtype Natural a = Natural Int
  deriving (Show, Generic, Typeable, Eq, Ord)
instance Num (Natural a) where
  (+) (Natural left) (Natural right) = verifyNaturalInt $! left + right
  (-) (Natural left) (Natural right) = verifyNaturalInt $! left - right
  (*) (Natural left) (Natural right) = verifyNaturalInt $! left * right
  abs v = v
  signum (Natural left) = verifyNaturalInt $ signum left
  fromInteger v = verifyNaturalInt (fromInteger v)

instance ToJSON (Natural Int)
instance FromJSON (Natural Int) where
  parseJSON = withScientific "Natural" $ \v-> return (verifyNaturalScientific v)
instance ToSchema (Natural Int) where
  declareNamedSchema _ = return $ NamedSchema (Just "Natural") $ mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 0
instance ToParamSchema (Natural Int) where
  toParamSchema _ = mempty
    & type_ ?~ SwaggerNumber
    & maximum_ ?~ fromIntegral (maxBound ::Int)
    & minimum_ ?~ 0
instance FromHttpApiData (Natural Int) where
  parseUrlPiece t =
    case TR.decimal t of
      Left _ -> Left "Natural: wrong value"
      Right (v, _) -> Right (verifyNaturalInt v)
  parseQueryParam t =
    case TR.decimal t of
      Left _ -> Left "Natural: wrong value"
      Right (v, _)-> Right (verifyNaturalInt v)
instance ToHttpApiData (Natural Int) where
  toUrlPiece (Natural v) = toUrlPiece v
  toQueryParam (Natural v) = toQueryParam v

verifyNaturalScientific:: Scientific -> (Natural a)
verifyNaturalScientific s =
  case toBoundedInteger s of
    Just v -> verifyNaturalInt v
    _ -> error "verifyNaturalScientific: wrong value"

verifyNaturalInt:: Int -> (Natural a)
verifyNaturalInt v =
  case () of
    _ | v >= 0 -> Natural v
    _ -> error "verifyNatural: wrong value"
