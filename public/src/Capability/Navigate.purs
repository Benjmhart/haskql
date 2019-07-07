-- | A capability representing the ability to move the user from place to place in the application. 
-- | Currently, the production monad implements hash-based routing, but that could easily be replaced 
-- | with another method (pushState, for example) without breaking any code outside of `Main`.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module Capability.Navigate where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Ref as Ref
import Data.Maybe(Maybe(..))
import Control.Monad.Trans.Class (lift)
import Model.Route (Route(..))
import Model.User (User)
import Halogen (HalogenM, liftEffect)
import Halogen.Router (pushRoute)
import Model.LocalStorage(deleteLocalStorage, tokenKey)

-- | This capability represents the ability to move around the application. The `navigate` function 
-- | should change the browser location, which will then notify our routing component. The `logout`
-- | function should clear any information associated with the user from the app and browser before
-- | redirecting them to the homepage.
class Monad m <= Navigate m where
  navigate :: Route -> m Unit
  logout :: Ref.Ref (Maybe User) -> m Unit

instance navigateEffect :: Navigate Effect where
  navigate = pushRoute
  logout currentUser = do
    Ref.write Nothing currentUser
    liftEffect $ deleteLocalStorage tokenKey
    navigate Home

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance navigateAff :: Navigate Aff where
  navigate = liftEffect <<< pushRoute
  logout currentUser = do
    liftEffect $ Ref.write Nothing currentUser
    liftEffect $ deleteLocalStorage tokenKey
    liftEffect $ navigate Home
    

instance navigateHalogenM :: Navigate m => Navigate (HalogenM st act slots msg m) where
  navigate = lift <<< navigate
  logout = lift <<< logout