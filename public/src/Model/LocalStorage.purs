module Model.LocalStorage where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
-- import Effect.Exception (try)

foreign import setItemString :: String -> String -> Effect Unit
foreign import getItemString :: String -> Effect String
foreign import removeItem :: String -> Effect Unit

newtype StorageKey = StorageKey String 

derive instance newtypeStorageKey :: Newtype StorageKey _

tokenKey = StorageKey "token"

setLocalStorage :: StorageKey -> String -> Effect Unit
setLocalStorage k v = liftEffect $ setItemString (unwrap k) v

getLocalStorage :: StorageKey -> Effect (Maybe String)
getLocalStorage k = liftEffect $ do
  val <- getItemString (unwrap k)
  case val of
    ("") -> pure Nothing
    a    -> pure $ Just a

deleteLocalStorage :: StorageKey -> Effect Unit
deleteLocalStorage k = liftEffect $ removeItem (unwrap k)