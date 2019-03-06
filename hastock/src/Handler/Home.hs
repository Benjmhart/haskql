{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE QuasiQuotes            #-}
module Handler.Home where

import Import
import Network.HTTP.Simple

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