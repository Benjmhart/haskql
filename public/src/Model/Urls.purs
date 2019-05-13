module Model.Urls where
  
import Data.Newtype (class Newtype)

newtype BaseUrl = BaseUrl String

newtype ApiUrl = ApiUrl String
derive instance newtypeApiUrl :: Newtype ApiUrl _