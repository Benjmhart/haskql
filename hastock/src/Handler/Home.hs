{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DataKinds              #-}

module Handler.Home where

import Import
import Network.HTTP.Simple
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified GraphQL as GQL
import qualified GraphQL.API as GQLAPI
import GraphQL.API ((:>))
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

-- | Nested Example
type Query' = GQLAPI.Object "Query" '[] '[GQLAPI.Field "me" User]
type User = GQLAPI.Object "User" '[] '[GQLAPI.Field "name" Text]

user :: GQLR.Handler IO User
user = pure name
  where
    name = pure "Mort"

query :: GQLR.Handler IO Query'
query = pure user

postHelloGraphR :: Handler Value
postHelloGraphR = do
  body <- requireJsonBody :: Handler Value
  print body
  let res =  GQL.interpretAnonymousQuery @Query' query "{ me { name }}"
  resJson <- liftIO $ toJSON <$> res
  return resJson

-- | Parameterized example
type Hello = GQLAPI.Object "Hello" '[]
'[ GQLAPI.Argument "who" Text :> GQLAPI.Field "greeting" Text ]

hello :: GQLR.Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

postHelloGraphParamR :: Handler Value
postHelloGraphParamR = do
  body <- requireJsonBody :: Handler Value
  print body
  let res =  GQL.interpretAnonymousQuery @Hello hello "{ greeting(who: \"oscar\") }"
  resJson <- liftIO $ toJSON <$> res
  return resJson