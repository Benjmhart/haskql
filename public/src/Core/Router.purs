module Core.Router (Input, State, Query(..), component) where

import Prelude

import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate)
import Capability.Now (class Now)
import Capability.Resource.FetchQuote (class FetchQuote)
import Model.Route (Route(..))
import Page.SymbolSearch as Home
import Page.Register as Register
import Core.Header as Header
import Control.Monad.Reader (class MonadAsk)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HelperLib as HL
import Halogen.Router as HR
import Model.Urls (ApiUrl)
import Core.Router.Styles (coreLayout, bodyContent)

-- coreLayoutSelector = 

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
type ChildQuery = Coproduct3
  Header.Query
  Home.Query
  -- Login.Query
  Register.Query
  -- Watched.Query

type ChildSlot = Either3
  Unit
  Unit
  Unit

component
  :: forall m r
   . MonadAff m
  -- => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Now m
  => LogMessages m
  => Navigate m
  => MonadAsk { apiUrl :: ApiUrl | r } m
  => FetchQuote m 
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
  render { route } = 
    HH.div [ HL.class_ coreLayout] [
      HH.div [ HL.class_ bodyContent ]
      [ HH.slot' CP.cp1 unit Header.component route absurd
      , case route of
          Home -> 
            HH.slot' CP.cp2 unit Home.component unit absurd
          -- Login -> 
          --   HH.slot' CP.cp2 unit Login.component unit absurd
          Register -> 
            HH.slot' CP.cp3 unit Register.component unit absurd
      ]
    ]