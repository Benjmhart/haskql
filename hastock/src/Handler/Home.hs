{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE DeriveGeneric          #-}

module Handler.Home where

import Import
import Network.HTTP.Simple
import Data.FileEmbed                   (embedFile)
import qualified Data.Aeson             as A
import Data.Yaml                        (decodeEither', ParseException)

getHomeR :: Handler ()
getHomeR = do
  sendFile "text/html" "static/dist/index.html"


data Secrets = Secrets { apiKey  :: Text
                       , secret1 :: Text
                       , secret2 :: Text
                       } deriving Show

instance FromJSON Secrets where
    parseJSON = A.withObject "Secrets" $ \v -> Secrets
        <$> v .: "apiKey"
        <*> v .: "secret1"
        <*> v .: "secret2"

data Error = Error { message :: Text
                   , statusCode :: Int
                   }
instance ToJSON Error where
  toJSON (Error m sC) = object ["message" .= m, "statusCode" .= sC]

getStocksQuoteR :: Text -> HandlerFor App Value
getStocksQuoteR stockSymbol = do
  print stockSymbol
  requestURL <- parseRequest "https://www.alphavantage.co/query"
  let secrets = (decodeEither' $(embedFile "config/secrets.yml")) :: Either ParseException Secrets
  makeRequest secrets requestURL
    where 
      makeRequest (Right secrets) rURL = do
        let request = setRequestQueryString [ ("function", Just "GLOBAL_QUOTE")
                                            , ("symbol", Just . encodeUtf8 $ stockSymbol)
                                            , ("apikey", Just . encodeUtf8 . apiKey $ secrets)
                                            ] 
                                            rURL
        response <- httpJSON $ request
        let gq = (getResponseBody $ response :: GlobalQuote)
        print gq
        let qr = toJSON $ (quoteField $ gq :: QuoteRecord)
        print qr
        return qr
      makeRequest (Left exception) _ = do
        print exception
        let errorResponse = toJSON $ Error "Server error" 500
        return errorResponse

data GlobalQuote = GlobalQuote { quoteField :: QuoteRecord } deriving (Eq, Show, Generic)
instance FromJSON GlobalQuote where
  parseJSON = A.withObject "GlobalQuote" $ \v -> GlobalQuote
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
  parseJSON = A.withObject "Quote" $ \v -> QuoteRecord
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
instance ToJSON QuoteRecord where
  toEncoding = A.genericToEncoding A.defaultOptions
