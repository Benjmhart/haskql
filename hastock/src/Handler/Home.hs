{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}

module Handler.Home where

import           Import

import            Handler.Quote.GetQuote         ( getQuote )
import            Model.User
import            Model.User                     ( UnvalidatedUser(..) )
import            Data.Aeson                     ( fromJSON )
import            Data.Aeson
import qualified  Data.Text                     as T
import            Data.Char
import            Yesod.Auth.Util.PasswordStore  ( makePassword )
import qualified  Web.JWT as JWT

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

postRegisterR :: HandlerFor App Value
postRegisterR = do
  body <- requireJsonBody :: Handler Value
  let unvalidatedUser = fromJSON body :: Result UnvalidatedUser
  let validatedUser =
        passwordValidator
          =<< emailValidator
          =<< nameValidator
          =<< unvalidatedUser
  case validatedUser of
    (Error str) -> do
      error str
    (Success vu) -> do
      hashedPassword <-
        liftIO $ ((flip makePassword) 10 . encodeUtf8 . password) $ vu
      print hashedPassword
      let validatedUser' = (flip makeValidatedUser $ hashedPassword) $ vu
      -- We Can't do inserts using insertKey because it breaks stuff
      key <- runDB $ insert validatedUser'
      --this successfully updates the DB
      let mySecret = JWT.secret "hello"
      let jwt = JWT.encodeSigned JWT.HS256 mySecret JWT.def
      let userResponseJSON = toJSON $ UserResponse jwt
      return userResponseJSON

data UserResponse = UserResponse { token :: Text }
  deriving (Generic)

instance ToJSON UserResponse where
  toJSON = genericToJSON defaultOptions

instance FromJSON UserResponse where
  parseJSON = genericParseJSON defaultOptions

-- TODO: handle non-unique entry error (make more readable)

makeValidatedUser :: UnvalidatedUser -> ByteString -> User
makeValidatedUser (UnvalidatedUser name email _) hashedPassword =
  User name email $ decodeUtf8 hashedPassword

weirdChars :: Char -> Bool
weirdChars char = T.any (== char) ";/\\(){}[]"

nameValidator :: UnvalidatedUser -> Result UnvalidatedUser
nameValidator user@(UnvalidatedUser name' _ _)
  | notLongerThan3 = Error "name needs to be longer than 3 characters"
  | hasWhitespace  = Error "name cannot contain only white space"
  | hasWeirdChars  = Error "suspicious characters in name"
  | otherwise      = Success user
 where
  notLongerThan3 = not . (3 <) . T.length $ name'
  hasWhitespace  = (T.any (== ' ')) $ name'
  hasWeirdChars  = T.any weirdChars name'

emailValidator :: UnvalidatedUser -> Result UnvalidatedUser
emailValidator user@(UnvalidatedUser _ email' _)
  | notLongerThan3 = Error "email needs to be longer than 3 characters"
  | hasWhitespace  = Error "email cannot contain only white space"
  | hasWeirdChars  = Error "suspicious characters in email"
  | not hasAt      = Error "needs a single @ sign in email"
  | not hasDots    = Error "needs a . in email"
  | otherwise      = Success user
 where
  notLongerThan3 = not . (3 <) . T.length $ email'
  hasWhitespace  = (T.any (== ' ')) $ email'
  hasWeirdChars  = T.any weirdChars email'
  hasAt          = (== 1) . T.length . (T.filter (== '@')) $ email'
  hasDots        = T.any (== '.') $ email'

passwordValidator :: UnvalidatedUser -> Result UnvalidatedUser
passwordValidator user@(UnvalidatedUser _ _ password')
  | notLongerThan3 = Error "password needs to be longer than 3 characters"
  | hasWhitespace = Error "password cannot contain only white space"
  | hasWeirdChars = Error "suspicious characters in password"
  | hasCorrectCharacters = Error
    "password needs uppercase, lowercase, special, and digit"
  | otherwise = Success user
 where
  notLongerThan3 = not . (6 <) . T.length $ password'
  hasWhitespace  = (T.any (== ' ')) $ password'
  hasWeirdChars  = T.any weirdChars password'
  hasCorrectCharacters =
    not
      .   and
      $   ((flip T.any) password')
      <$> [isUpper, isLower, isPunctuation, isDigit]
