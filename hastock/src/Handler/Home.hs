{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE DeriveGeneric          #-}

module Handler.Home where

import Import
import Network.HTTP.Simple
import Data.Text (Text, toTitle)
import Data.Text.Encoding(decodeASCII)
import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Data.Char (isAlpha)
import qualified GraphQL as GQL
import qualified GraphQL.API as GQLAPI
import GraphQL.API ((:>))
import qualified GraphQL.Value as GQLV
import GraphQL.Value.ToValue as TV
import qualified GraphQL.Resolver as GQLR -- we mostly care about Handler

getHomeR :: HandlerFor App Value
getHomeR  = return $ object ["symbol" .= ("Hello World" :: Value)]


getStocksQuoteR :: Text -> HandlerFor App Value
getStocksQuoteR symbol = do
    requestURL <- parseRequest "https://www.alphavantage.co/query"
    let request = setRequestQueryString [ ("function", Just "GLOBAL_QUOTE")
                                        , ("symbol", Just . encodeUtf8 $ symbol)
                                        , ("apikey", Just "V31DZE26DJHKTH7A")
                                        ] 
                                        requestURL
    response <- httpJSON $ request
    return $ getResponseBody response

type Hello = GQLAPI.Object "Hello" '[]
  '[ GQLAPI.Argument "who" Text :> GQLAPI.Field "greeting" Text ]


  -- this is failing because it isn't the correct type motherfucker!
type QuoteGQL = GQLAPI.Object "QuoteGQL" '[]
  '[ GQLAPI.Argument "symbol" Text :> GQLAPI.Field "globalQuote" GlobalQuote ]
  
data GlobalQuote = GlobalQuote { quoteField :: Maybe QuoteRecord } deriving (Eq, Show, Generic)
instance FromJSON GlobalQuote where
  parseJSON = withObject "GlobalQuote" $ \v -> GlobalQuote
      <$> v .: "Global Quote"

data QuoteRecord = QuoteRecord  { symbol              :: Text
                                , open                :: Text
                                , high                :: Text
                                , low                 :: Text
                                , price               :: Text
                                , volume              :: Text
                                , latestTradingDay    :: Text
                                , previousClose       :: Text
                                , change              :: Text
                                , changePercent       :: Text
                                } deriving (Eq, Show, Generic)

instance FromJSON QuoteRecord where 
  parseJSON = withObject "GlobalQuote" $ \v -> Quote
    <$> v .: "01. symbol"
    <*> v .: "02. open"
    <*> v .: "03. high"
    <*> v .: "04. low"
    <*> v .: "05. price"
    <*> v .: "06. volume"
    <*> v .: "07. latest trading day"
    <*> v .: "08. previous close"
    <*> v .: "09. change"
    <*> v .: "10. change percent"

type QuoteM = GQLAPI.Object "QuoteGQL" '[]
    '[ GQLAPI.Argument "symbol" Text :> GQLAPI.Field "globalQuote" (Maybe QuoteR) ]

type QuoteR =  GQLAPI.Object "QuoteR" '[] 
  '[ GQLAPI.Field  "symbol"           Text
   , GQLAPI.Field  "open"             Text
   , GQLAPI.Field  "high"             Text
   , GQLAPI.Field  "low"              Text
   , GQLAPI.Field  "price"            Text
   , GQLAPI.Field  "volume"           Text
   , GQLAPI.Field  "latestTradingDay" Text
   , GQLAPI.Field  "previousClose"    Text
   , GQLAPI.Field  "change"           Text
   , GQLAPI.Field  "changePercent"    Text
  ]

makeQuoteR :: QuoteRecord -> Maybe (GQLV.Object' GQLV.ConstScalar)
makeQuoteR qr = GQLV.objectFromList 
  [ ( "symbol", TV.toValue $ symbol qr)
  , ( "open" ,  TV.toValue $ open qr) 
  , ( "high" ,  TV.toValue $ high qr) 
  , ( "low" ,   TV.toValue $ low qr) 
  , ( "price" , TV.toValue $ price qr) 
  , ( "volume" , TV.toValue $ volume qr) 
  , ( "latestTradingDay" , TV.toValue $ latestTradingDay qr) 
  , ( "previousClose" , TV.toValue $ previousClose qr) 
  , ( "change" , TV.toValue $ change qr) 
  , ( "changePercent" , TV.toValue $ changePercent qr) 
  ]

quote :: GQLR.Handler IO QuoteR
quote = pure (\symbol -> do 
  -- print $ "symbol: " <> "\"" <> symbol <> "\""
  requestURL <- parseRequest "https://www.alphavantage.co/query"
  let request = setRequestQueryString [ ("function", Just "GLOBAL_QUOTE")
                                      , ("symbol", Just . encodeUtf8 $ symbol)
                                      , ("apikey", Just "V31DZE26DJHKTH7A")
                                      ] 
                                      requestURL
  response <- liftIO $ httpJSON $ request
  let quoter = makeQuoteR =<< (quoteField =<< getResponseBody response)
  return quoter
  )

hello :: GQLR.Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

data QueryType = Hello | Quote -- | User etc...
  deriving (Eq, Ord, Read, Show)

-- postHelloGraphR = GQL.interpretAnonymousQuery @Hello hello
postHelloGraphR :: Handler Value
postHelloGraphR = do
  body <- requireJsonBody :: Handler Value
  let (Just qString) = getQueryString body
  let mQueryType     = getRootQuery qString 
  let queryArgs      = getQueryArgs qString
  -- print mQueryType
  -- TODO remove redundant lines by assigning res to the result of the case  expression
  case mQueryType of
    (Just Hello) -> do 
      let res = GQL.interpretAnonymousQuery @Hello hello queryArgs
      resJson <- liftIO $ toJSON <$> res
      return resJson
    (Just Quote) -> do 
      let res = GQL.interpretAnonymousQuery @QuoteR quote queryArgs
      --print $ "GQL RESPONSE OBJECT: " 
      resJson <- liftIO $ toJSON <$>res
      return resJson
    _ -> do
      let resJson = (String "Invalid Query")
      return resJson
    
-- TODO - move these into a helper library for parsing
getQueryArgs :: Text -> Text 
getQueryArgs = dropWhile (not . (=='{')) . drop 1 . dropEnd 1

getQueryString :: Value -> Maybe Text
getQueryString (Object (m)) = do 
  let (Just (String a)) = listToMaybe $ toList m
  return a
getQueryString _ = Nothing

getFirstWord :: Text -> Text
getFirstWord = toTitle . takeWhile (isAlpha) . dropWhile (not . isAlpha)

getRootQuery :: Text -> Maybe QueryType
getRootQuery tx = readMaybe . unpack =<< (getFirstWord <$> Just tx)