module Model.Token where
  
import Prelude (class Show)
import Simple.JSON(class ReadForeign)
import Data.Newtype (class Newtype)

newtype Token = Token String
derive newtype instance readForeignToken :: ReadForeign Token
derive newtype instance showToken :: Show Token
derive instance newtypeToken :: Newtype Token _