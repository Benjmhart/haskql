module Model.LocalStorage where

import Prelude
import Data.Newtype (class Newtype, unwrap)
import Effect (Effect)
-- import Effect.Exception (try)

foreign import setItemString :: String -> String -> Effect Unit
foreign import getItemString :: String -> Effect String

newtype StorageKey = StorageKey String 

derive instance newtypeStorageKey :: Newtype StorageKey _

setLocalStorage :: StorageKey -> String -> Effect Unit
setLocalStorage k v = setItemString (unwrap k) v

getLocalStorage :: StorageKey -> Effect String
getLocalStorage k = getItemString (unwrap k)