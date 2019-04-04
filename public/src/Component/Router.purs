module Component.Router where

import Prelude

import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate)
import Capability.Now (class Now)
-- import Conduit.Capability.Resource.Article (class ManageArticle)
-- import Conduit.Capability.Resource.Comment (class ManageComment)
-- import Conduit.Capability.Resource.Tag (class ManageTag)
-- import Conduit.Capability.Resource.User (class ManageUser)
-- import Conduit.Data.Profile (Profile)
import Model.Route (Route(..))
-- import Conduit.Page.Editor as Editor
import Component.SymbolSearch as Home
import Component.Register as Register
-- import Conduit.Page.Login as Login
-- import Conduit.Page.Profile (Tab(..))
-- import Conduit.Page.Profile as Profile
-- import Conduit.Page.Register as Register
-- import Conduit.Page.Settings as Settings
-- import Conduit.Page.ViewArticle as ViewArticle
import Control.Monad.Reader (class MonadAsk)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH

type State =
  { route :: Route }

data Query a
  = Navigate Route a

type Input =
  Maybe Route

-- If you haven't seen nested `Coproduct` or `Either` before, or you haven't worked with multiple types of
-- child component, then these types are probably confusing. They're a little tedious to define and are
-- being removed in favor of a much nicer mechanism in Halogen 5, but are necessary in Halogen 4.
-- 
-- For a detailed explanation of what's going on here, please see this issue:
-- https://github.com/thomashoneyman/purescript-halogen-realworld/issues/20
type ChildQuery = Coproduct2
  Home.Query
  -- Login.Query
  Register.Query
  -- Watched.Query

type ChildSlot = Either2
  Unit
  Unit

component
  :: forall m r
   . MonadAff m
  -- => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => MonadAsk { apiUrl :: String | r } m
  -- => ManageUser m
  -- => ManageArticle m
  -- => ManageComment m
  -- => ManageTag m
  => H.Component HH.HTML Query Input Void m
component =
  H.parentComponent
    { initialState: \initialRoute -> { route: fromMaybe Home initialRoute } 
    , render
    , eval
    , receiver: const Nothing
    }

  where 

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate dest a) = do
    { route } <- H.get 
    when (route /= dest) do
      H.modify_ _ { route = dest }
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } = case route of
    Home -> 
      HH.slot' CP.cp1 unit Home.component unit absurd
    -- Login -> 
    --   HH.slot' CP.cp2 unit Login.component unit absurd
    Register -> 
      HH.slot' CP.cp2 unit Register.component unit absurd