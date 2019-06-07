module Model.UserPostBody where

import Prelude (otherwise, (==), (/=))
import Data.Newtype (class Newtype)
import Data.Either (Either(..))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode (class EncodeJson, encodeJson, (:=), (:=?), (~>))
-- TODO make a newType/dataType wrapper for Quote

newtype UserPostBody 
  = UserPostBody
    { name     :: String
    , email    :: String
    , password :: String
    }

derive instance newtypeUserPostBody :: Newtype UserPostBody _
instance encodeJsonUserPostBody :: EncodeJson UserPostBody where
  encodeJson (UserPostBody x) =
    "name" := x.name 
      ~> "email" := x.email 
      ~> "password" := x.password -- optional field
      ~> jsonEmptyObject


validateUserPostBody :: UserPostBody -> String -> Either String UserPostBody
validateUserPostBody (UserPostBody body) validatePassword
  | body.name     == "" = Left "You must enter a name"
  | body.password == "" = Left "You must enter an password"
  | body.email    == "" = Left "You must enter an email"
  | body.password /= validatePassword = Left "Passwords do not match"
  | otherwise           = Right (UserPostBody body)