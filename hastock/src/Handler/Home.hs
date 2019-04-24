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
import Model.User             (UnvalidatedUser(..))
import Database.Persist.Class (insert)
import Data.Aeson             (fromJSON)
import Data.Aeson
import Data.Text as T
import Data.Char
import Yesod.Auth.Util.PasswordStore
-- import Crypto.KDF.BCrypt

getHomeR :: Handler ()
getHomeR = sendFile "text/html" "static/index.html"

getHomeJSR :: Handler ()
getHomeJSR = sendFile "text/javascript" "static/index.js"


getQuoteR :: Text -> HandlerFor App Value
getQuoteR = getQuote

getInsertR :: Text -> HandlerFor App Value
getInsertR = \stockSymbol -> do
  let Entity userId user = sampleUser
  result <- runDB $ insertKey userId user
  liftIO $ print result
  return $ toJSON ("" :: String)

postRegisterR :: HandlerFor App ()
postRegisterR = do
    body <- requireJsonBody :: Handler Value
    let unvalidatedUser = fromJSON body :: Result UnvalidatedUser
    let validatedUser = passwordValidator =<< emailValidator =<< nameValidator =<< unvalidatedUser
    let hashedPassword = ((flip makePassword) 10 . encodeUtf8 . password) <$> validatedUser
    let validatedUser' = (liftM2 makeValidatedUser) validatedUser hashedPassword
    print validatedUser'
  -- Encrypt Password
  -- Insert
  -- Generate JWT
  -- Return JWT

makeValidatedUser :: UnvalidatedUser -> ByteString -> Result User
makeValidatedUser (UnvalidatedUser name email _) hashedPassword
  = Success $ User name email $ decodeUtf8 hashedPassword 

weirdChars :: Char -> Bool
weirdChars char = T.any (== char) ";/\\(){}[]"

nameValidator :: UnvalidatedUser -> Result UnvalidatedUser
nameValidator user@(UnvalidatedUser name' _ _)
  | notLongerThan3 = Error "name needs to be longer than 3 characters"
  | hasWhitespace = Error "name cannot contain only white space"
  | hasWeirdChars = Error "suspicious characters in name"
  | otherwise = Success user
    where
      notLongerThan3 = not . (3<) . T.length $ name'
      hasWhitespace = (T.any (== ' ')) $ name'
      hasWeirdChars = T.any weirdChars name'

emailValidator :: UnvalidatedUser -> Result UnvalidatedUser
emailValidator user@(UnvalidatedUser _ email' _)
  | notLongerThan3 = Error "email needs to be longer than 3 characters"
  | hasWhitespace = Error "email cannot contain only white space"
  | hasWeirdChars = Error "suspicious characters in email"
  | not hasAt = Error "needs a single @ sign in email"
  | not hasDots = Error "needs a . in email"
  | otherwise = Success user
    where
      notLongerThan3 = not . (3<) . T.length $ email'
      hasWhitespace = (T.any (== ' ')) $ email'
      hasWeirdChars = T.any weirdChars email'
      hasAt = (== 1) . T.length . (T.filter (== '@')) $ email'
      hasDots = T.any (== '.') $ email'

passwordValidator :: UnvalidatedUser -> Result UnvalidatedUser
passwordValidator user@(UnvalidatedUser _ _ password')
  | notLongerThan3 = Error "password needs to be longer than 3 characters"
  | hasWhitespace = Error "password cannot contain only white space"
  | hasWeirdChars = Error "suspicious characters in password"
  | hasCorrectCharacters = Error "password needs uppercase, lowercase, special, and digit"
  | otherwise = Success user
    where
      notLongerThan3 = not . (6<) . T.length $ password'
      hasWhitespace = (T.any (== ' ')) $ password'
      hasWeirdChars = T.any weirdChars password'
      hasCorrectCharacters = not
                           . and 
                           $ ((flip T.any) password')
                           <$> [isUpper, isLower, isPunctuation, isDigit]