module Model.User where
  
import Prelude (class Show)
import Data.Newtype (class Newtype)

newtype User 
  = User 
    {  userName :: String
    }

derive instance newtypeUser :: Newtype User _
derive newtype instance showUser :: Show User