{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}

module Handler.Quote.GetQuote (getQuote) where

import Import
import Network.HTTP.Simple
import Data.FileEmbed           (embedFile)
import Data.Yaml                (decodeEither', ParseException)

import qualified Model.Secrets  as Secret
import qualified Model.Quote    as Quote

getQuote :: Text -> HandlerFor App Value
getQuote stockSymbol = do
  print stockSymbol
  requestURL <- parseRequest "https://www.alphavantage.co/query"
  let secrets = (decodeEither' $(embedFile "config/secrets.yml")) :: Either ParseException Secret.Secrets
  print secrets
  makeRequest secrets requestURL
    where 
      makeRequest (Right secrets) rURL = do
        let 
          request = setRequestQueryString 
                    [ ("function", Just "GLOBAL_QUOTE")
                    , ("symbol", Just . encodeUtf8 $ stockSymbol)
                    , ("apikey", Just . encodeUtf8 . Secret.apiKey $ secrets)
                    ] 
                    rURL
        response <- httpJSON $ request
        let gq = (getResponseBody $ response :: Quote.GlobalQuote)
        let qr = toJSON $ (Quote.quoteField $ gq :: Quote.QuoteRecord)
        print qr
        return qr
      makeRequest (Left exception) _ = do
        print exception
        let errorResponse = toJSON $ Secret.Error "Server error" 500
        return errorResponse