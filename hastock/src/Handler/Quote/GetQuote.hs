{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}

module Handler.Quote.GetQuote (getQuote) where

import Import
import Network.HTTP.Simple

import qualified Model.Quote    as Quote

getQuote :: Text -> HandlerFor App Value
getQuote stockSymbol = do
  print stockSymbol
  app <- getYesod
  let apiKey = alphaVantageApiKey . appSettings $ app
  requestURL <- parseRequest "https://www.alphavantage.co/query"
  makeRequest apiKey requestURL
    where 
      makeRequest apiKey rURL = do
        let 
          request = setRequestQueryString 
                    [ ("function", Just "GLOBAL_QUOTE")
                    , ("symbol", Just . encodeUtf8 $ stockSymbol)
                    , ("apikey", Just . encodeUtf8 $ apiKey)
                    ] 
                    rURL
        response <- httpJSON $ request
        let gq = (getResponseBody $ response :: Quote.GlobalQuote)
        let qr = toJSON $ (Quote.quoteField $ gq :: Quote.QuoteRecord)
        print qr
        return qr