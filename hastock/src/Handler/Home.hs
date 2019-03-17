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

module Handler.Home where

import Import
import Network.HTTP.Simple
import Data.Text (Text, toTitle)
import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Data.Char (isAlpha)
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

type Hello = GQLAPI.Object "Hello" '[]
  '[ GQLAPI.Argument "who" Text :> GQLAPI.Field "greeting" Text ]

hello :: GQLR.Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

data QueryType = Hello  -- | Quote etc...
  deriving (Eq, Ord, Read, Show)

-- postHelloGraphR = GQL.interpretAnonymousQuery @Hello hello
postHelloGraphR :: Handler Value
postHelloGraphR = do
  body <- requireJsonBody :: Handler Value
  let (Just qString) = getQueryString body
  let mQueryType     = getRootQuery qString 
  let queryArgs      = getQueryArgs qString
  print mQueryType
  case mQueryType of
    (Just Hello) -> do 
      let res = GQL.interpretAnonymousQuery @Hello hello queryArgs
      resJson <- liftIO $ toJSON <$> res
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