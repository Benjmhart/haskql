module Capability.Resource.Register where

import Prelude ((<<<), class Monad)

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Data.Maybe(Maybe)
import Data.Either (Either)
import Model.Quote (Quote)
import Model.UserPostBody(UserPostBody(..))
import Model.Token(Token(..))
import Model.UserResponseBody (UserResponseBody)


class Monad m <= Register m where
  register :: UserPostBody -> m (Either String (Maybe UserResponseBody))

instance registerHalogenM :: Register m => Register (HalogenM st act slots msg m) where
  register = lift <<< register
