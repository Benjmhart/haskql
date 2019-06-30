{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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
import           Yesod.Auth.Util.PasswordStore  ( makePassword, verifyPassword )
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

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Error _) = Nothing
resultToMaybe (Success value) = Just value

makeJWTClaimsSet :: Key User -> JWT.JWTClaimsSet
makeJWTClaimsSet dbEntity = JWT.def
          { JWT.unregisteredClaims =
            Map.fromList [("id", String . tshow . fromSqlKey $ dbEntity)]
          }

getIdFromJWT :: Text -> Maybe Int64
getIdFromJWT jwt = do 
  parsedJWT <- JWT.decodeAndVerifySignature mySecret jwt
  dbRecordKeyJSON <- lookup "id" . JWT.unregisteredClaims $ JWT.claims parsedJWT
  dbRecordKey <- resultToMaybe . fromJSON $ dbRecordKeyJSON
  readIntegral dbRecordKey

getUserR :: Text -> HandlerFor App Value
getUserR jwt = 
      case getIdFromJWT jwt of
        Nothing -> sendResponseStatus error500 ("error parsing credentials, please try logging in" :: T.Text)
        Just parsedId -> do
          userEntity <- runDB $ do
            get (toSqlKey parsedId :: Key User)
          case userEntity of
              Nothing -> sendResponseStatus error404 ("invalid credentials, please try logging in" :: T.Text)
              Just user -> return $ toJSON user



postLoginR :: HandlerFor App Value
postLoginR = do
  body <- requireCheckJsonBody :: Handler Value
  case (fromJSON body :: Result LoginInfo) of
    Error s               ->  sendResponseStatus error500 $ toJSON s
    Success loginDetails  ->  runDB $ do
      userRecord <- getBy $ UniqueEmail $ loginEmail loginDetails
      case userRecord of
        Nothing -> sendResponseStatus error404 $ toJSON ("email does not exist, please register or try again" :: T.Text)
        Just entity@(Entity _ dbUser) -> if (invalidPassword)
          then sendResponseStatus error404 $ toJSON ("invalid password, please try again" :: T.Text)
          else return $ toJSON $ UserResponse jwt $ userName dbUser
            where
              invalidPassword = not $ verifyPassword (encodeUtf8 . loginPassword $ loginDetails) (encodeUtf8 . userPassword $ dbUser)
              -- cs = JWT.def { JWT.unregisteredClaims =
              --   Map.fromList [("id", String . tshow . fromSqlKey $ )]
              -- }
              jwt = JWT.encodeSigned JWT.HS256 mySecret $ makeJWTClaimsSet $ entityKey entity


-- TODO: Make sure username and email are lowercased
postRegisterR :: HandlerFor App Value
postRegisterR = do
  body <- requireCheckJsonBody :: Handler Value
  let unvalidatedUser  =  fromJSON body :: Result UnvalidatedUser
  let validatedUser    =  passwordValidator
                      =<< emailValidator
                      =<< nameValidator
                      =<< unvalidatedUser
  case validatedUser of
    (Error   str) -> sendResponseStatus error500 $ toJSON str
    (Success vu ) -> do
      hashedPassword <- liftIO
        $ (flip makePassword 10 . encodeUtf8 . password) vu
      dbEntity <- runDB $ insert $ makeValidatedUser hashedPassword vu
      return $ toJSON 
             $ UserResponse 
                (JWT.encodeSigned JWT.HS256 mySecret (makeJWTClaimsSet dbEntity))
             $ Model.User.name vu

makeValidatedUser :: ByteString -> UnvalidatedUser -> User
makeValidatedUser hashedPassword (UnvalidatedUser uvName uvEmail _) =
  User uvName uvEmail $ decodeUtf8 hashedPassword

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
