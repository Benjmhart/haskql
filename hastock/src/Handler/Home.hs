{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE DataKinds              #-}

module Handler.Home where

import           Import

import           Handler.Quote.GetQuote         ( getQuote )
import           Model.User
import           Data.Aeson
import           Database.Persist.Sql           ( fromSqlKey
                                                , toSqlKey
                                                )
import qualified Data.Text                     as T
import           Data.Char
import qualified Data.Map                      as Map
import           Yesod.Auth.Util.PasswordStore  ( makePassword )
import qualified Web.JWT                       as JWT

mySecret :: JWT.Secret
mySecret = JWT.secret "hello"

getHomeR :: Handler ()
getHomeR = sendFile "text/html" "static/index.html"

getHomeJSR :: Handler ()
getHomeJSR = sendFile "text/javascript" "static/index.js"


getQuoteR :: Text -> HandlerFor App Value
getQuoteR = getQuote

error500 :: Status
error500 = Status 500 "server error"

error404 :: Status
error404 = Status 404 "client not found"

getUserR :: Text -> HandlerFor App Value
getUserR jwt = do
  let parsedJWT = JWT.decodeAndVerifySignature (JWT.secret "hello") jwt
  case parsedJWT of
    Nothing       -> sendResponseStatus error500 $ toJSON ("credentials in invalid format" :: T.Text)
    Just verified -> do
      let id = lookup "id" . JWT.unregisteredClaims $ JWT.claims verified --TODO: Add proper error message
      case id of
        Nothing -> sendResponseStatus error500 $ toJSON ("credentials missing information " :: T.Text)
        Just id -> do
          let parsedId = (fromJSON id) :: Result String
          case parsedId of
            Error s -> do
              return $ toJSON s
            Success value -> do
              let maybeKey = readIntegral value :: Maybe Int64
              case maybeKey of
                Just key -> do
                  runDB $ do
                    userRecord <- get (toSqlKey key :: Key User)
                    case userRecord of
                      Just user -> do 
                        let response = toJSON userRecord
                        return response
                      Nothing -> sendResponseStatus error404 $ toJSON ("couldn't validate, user doesn't exist" :: T.Text)
                Nothing -> sendResponseStatus error404 $ toJSON ("couldn't validate, invalid credentials" :: T.Text)

-- TODO: Make sure username and email are lowercased
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
    (Error   str) -> sendResponseStatus error500 $ toJSON str
    (Success vu ) -> do
      hashedPassword <- liftIO
        $ (flip makePassword 10 . encodeUtf8 . password) vu
      print hashedPassword
      let validatedUser' = (flip makeValidatedUser $ hashedPassword) vu
      -- We Can't do inserts using insertKey because it breaks stuff
      key <- runDB $ insert validatedUser'
      --this successfully updates the DB
      let
        cs = JWT.def
          { JWT.unregisteredClaims =
            Map.fromList [("id", String . tshow . fromSqlKey $ key)]
          }
      let jwt              = JWT.encodeSigned JWT.HS256 mySecret cs
      let userResponseJSON = toJSON $ UserResponse jwt $ Model.User.name vu
      return userResponseJSON

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
