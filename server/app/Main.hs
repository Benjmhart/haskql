{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import           Web.Scotty

fetchQuote :: Text -> IO (Value)
fetchQuote symbol = do
    requestURL <- parseRequest "https://www.alphavantage.co/query"
    let request = setRequestQueryString [ ("function", Just "GLOBAL_QUOTE")
                                        , ("symbol", Just . encodeUtf8 $ symbol)
                                        , ("apikey", Just "V31DZE26DJHKTH7A")
                                        ] 
                                        requestURL
    response <- httpJSON request
    return $ getResponseBody response

    
main = scotty 3000 $
  get "/quote/:symbol" $ do
    symbol <- param "symbol"
    jsonPayload <- liftIO $ fetchQuote symbol
    json $ jsonPayload
    