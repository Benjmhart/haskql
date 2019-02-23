{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

-- ?function=TIME_SERIES_INTRADAY&symbol=MSFT&interval=5min&apikey=demo
main :: IO ()
main = do
    requestURL <- parseRequest "https://www.alphavantage.co/query"
    let request = setRequestQueryString [ ("function", Just "GLOBAL_QUOTE")
                                        , ("symbol", Just "MSFT")
                                        , ("apikey", Just "V31DZE26DJHKTH7A")
                                        ] 
                                        requestURL
    response <- httpJSON request
    putStrLn $ ("The status code was: " ++) . tshow . getResponseStatusCode $ response
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)


