module Core.Router where

import Prelude

import Capability.Log (class Log)
import Capability.Navigate (class Navigate)
import Capability.Now (class Now)
import Capability.Resource.FetchQuote (class FetchQuote)
import Capability.Resource.Register (class Register)
import Model.Route (Route(..))
import Page.SymbolSearch as Home
import Page.Register as Register
import Core.Header as Header
import Control.Monad.Reader (class MonadAsk)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HelperLib as HL
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

type ChildSlots = 
  ( header :: HL.OpaqueChildSlot Unit
  , home :: HL.OpaqueChildSlot Unit
  , register :: HL.OpaqueChildSlot Unit
)

component
  :: forall m r
   . MonadAff m
  -- => MonadAsk { currentUser :: Ref (Maybe Profile) | r } m
  => Now m
  => Log m
  => Navigate m
  => MonadAsk { apiUrl :: ApiUrl | r } m
  => Register m
  => FetchQuote m 
  -- => ManageUser m
  -- => ManageArticle m
  -- => ManageComment m
  -- => ManageTag m
  => H.Component HH.HTML Query (Maybe Route) Void m
component =
  H.mkComponent
    { initialState: \initialRoute -> { route: fromMaybe Home initialRoute } 
    , render
    , eval: H.mkEval $ H.defaultEval 
      { handleQuery = handleQuery
      }
    }

  where 

  handleQuery :: forall a. Query a -> H.HalogenM State Void ChildSlots Void m (Maybe a)
  handleQuery (Navigate dest a) = do
    { route } <- H.get 
    H.modify_ _ { route = dest }
    pure (Just a)

  render :: State -> H.ComponentHTML Void ChildSlots m
  render { route } = 
    HH.div [ HL.class_ coreLayout] [
      HH.div [ HL.class_ bodyContent ]
      [ HH.slot (SProxy :: _ "header") unit Header.component route absurd
      , case route of
          Home -> 
            HH.slot (SProxy :: _ "home") unit Home.component unit absurd
          -- Login -> 
          --   HH.slot' CP.cp2 unit Login.component unit absurd
          Register -> 
            HH.slot (SProxy :: _ "register") unit Register.component unit absurd
      ]
    ]