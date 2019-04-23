{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}

module Handler.Home where

import Import

import Handler.Quote.GetQuote (getQuote)
import Model.User

getHomeR :: Handler ()
getHomeR = sendFile "text/html" "static/index.html"

getHomeJSR :: Handler ()
getHomeJSR = sendFile "text/javascript" "static/index.js"


getQuoteR :: Text -> HandlerFor App Value
getQuoteR = getQuote

insertR :: Text -> HandlerFor App Value
insertR = \stockSymbol -> do
  let Entity userId user = sampleUser
  result <- runDB $ insertKey userId user
  liftIO $ print result
  return $ toJSON ("" :: String)


