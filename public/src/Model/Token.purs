module Model.Token where
  
import Prelude
import Simple.JSON(class ReadForeign)
import Data.Newtype (class Newtype)

newtype Token = Token 
  { userResponseToken :: String
  , userResponseName  :: String
  }
derive newtype instance readForeignToken :: ReadForeign Token
derive newtype instance showToken :: Show Token
derive instance newtypeToken :: Newtype Token _