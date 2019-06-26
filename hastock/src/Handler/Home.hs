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

getIdFromJWT :: Text -> Maybe Value
getIdFromJWT jwt = do 
  parsedJWT <- JWT.decodeAndVerifySignature (JWT.secret "hello") jwt
  dbRecordKeyJSON <- lookup "id" . JWT.unregisteredClaims $ JWT.claims parsedJWT
  return dbRecordKeyJSON

-- getUserByID :: String -> HandlerFor App (Maybe User)
-- getUserByID id = do
--   -- parsedId :: Maybe Int64
--   parsedId <- readIntegral id
--   userEntity <- runDB $ do
--     get (toSqlKey parsedId :: Key User)
--   userEntity

getUserR :: Text -> HandlerFor App Value
getUserR jwt = 
      case getIdFromJWT jwt of
        Nothing -> sendResponseStatus error500 ("error parsing credentials, please try logging in" :: T.Text)
        Just dbRecordKey -> case (fromJSON dbRecordKey :: Result String) of
          Error   s     -> sendResponseStatus error500 $ toJSON s
          Success value -> case (readIntegral value) of
              Nothing -> sendResponseStatus error404 ("invalid credentials, please try logging in" :: T.Text)
              Just parsedId -> do
                userEntity <- runDB $ do
                  get (toSqlKey parsedId :: Key User)
                case userEntity of
                  Nothing -> sendResponseStatus error404 ("invalid credentials, please try logging in" :: T.Text)
                  Just user -> return $ toJSON user
            -- \user -> case user of
            --   Nothing -> sendResponseStatus error404 ("invalid credentials, please try logging in" :: T.Text)
            --   Just user -> return $ toJSON user
            -- <$> getUserByID value
            
            -- case getUserByID value of
            -- Nothing -> sendResponseStatus error404
            --     $ toJSON ("invalid credentials, please try logging in" :: T.Text)
            -- Just user -> return . toJSON $ user
            -- case (readIntegral value :: Maybe Int64) of
            --   Just key -> runDB $ do
            --     userRecord <- get (toSqlKey key :: Key User)
            --     case userRecord of
            --       Just user -> return $ toJSON user
            --       Nothing   -> sendResponseStatus error404 $ toJSON
            --         ("couldn't validate, user doesn't exist" :: T.Text)
            --   Nothing -> sendResponseStatus error404
            --     $ toJSON ("couldn't validate, invalid credentials" :: T.Text)

postLoginR :: HandlerFor App Value
postLoginR = do
  body <- requireCheckJsonBody :: Handler Value
  print body
  let loginInfo = fromJSON body :: Result LoginInfo
  case loginInfo of
    Error s       ->  sendResponseStatus error500 $ toJSON s
    Success loginDetails  ->  runDB $ do
      userRecord <- getBy $ UniqueEmail $ loginEmail loginDetails
      case userRecord of
        Nothing -> sendResponseStatus error404 $ toJSON ("email does not exist, please register or try again" :: T.Text)
        Just entity@(Entity _ dbUser) -> do
          let verification = verifyPassword (encodeUtf8 . loginPassword $ loginDetails) (encodeUtf8 . userPassword $ dbUser)
          if (not verification)
          then sendResponseStatus error404 $ toJSON ("invalid password, please try again" :: T.Text)
          else do
            let cs = JWT.def { JWT.unregisteredClaims =
                               Map.fromList [("id", String . tshow . fromSqlKey $ entityKey entity)]
                             }
            let jwt = JWT.encodeSigned JWT.HS256 mySecret cs
            return $ toJSON $ UserResponse jwt $ userName dbUser

-- TODO: Make sure username and email are lowercased
postRegisterR :: HandlerFor App Value
postRegisterR = do
  body <- requireCheckJsonBody :: Handler Value
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
      dbEntity <- runDB $ insert $ (flip makeValidatedUser $ hashedPassword) vu
      let
        cs = JWT.def
          { JWT.unregisteredClaims =
            Map.fromList [("id", String . tshow . fromSqlKey $ dbEntity)]
          }
      let jwt = JWT.encodeSigned JWT.HS256 mySecret cs
      return $ toJSON $ UserResponse jwt $ Model.User.name vu

makeValidatedUser :: UnvalidatedUser -> ByteString -> User
makeValidatedUser (UnvalidatedUser uvName uvEmail _) hashedPassword =
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
