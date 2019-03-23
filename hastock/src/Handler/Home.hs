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
import qualified Data.Aeson         as A

getHomeR :: Handler ()
getHomeR = do
  sendFile "text/html" "static/dist/index.html"


getStocksQuoteR :: Text -> HandlerFor App Value
getStocksQuoteR stockSymbol = do
  print stockSymbol
  requestURL <- parseRequest "https://www.alphavantage.co/query"
  let request = setRequestQueryString [ ("function", Just "GLOBAL_QUOTE")
                                      , ("symbol", Just . encodeUtf8 $ stockSymbol)
                                      , ("apikey", Just "V31DZE26DJHKTH7A")
                                      ] 
                                      requestURL
  response <- httpJSON $ request
  let gq = (getResponseBody $ response :: GlobalQuote)
  print gq
  let qr = toJSON $ (quoteField $ gq :: QuoteRecord)
  print qr
  return qr

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
