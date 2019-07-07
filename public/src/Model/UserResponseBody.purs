module Model.UserResponseBody where

import Data.Symbol (SProxy(..))
import Data.Lens(Lens)
import Data.Lens.Record (prop)

type UserResponseBody = 
  { userResponseToken :: String
  , userResponseName :: String
  }

_token = prop (SProxy :: SProxy "userResponseToken")
  :: forall a b r. Lens { userResponseToken :: a | r } { userResponseToken :: b | r } a b

_name = prop (SProxy :: SProxy "userResponseName")
  :: forall a b r. Lens { userResponseName :: a | r } { userResponseName :: b | r } a b