module Component.Register (State, Query(..), component) where

import Prelude (type (~>), Unit, Void, bind, const, discard, pure, show, ($), (<<<), (<>), (=<<))

import Data.Array (concat)
import Data.Maybe (Maybe(..), isNothing )
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Simple.JSON as JSON
import Web.Event.Event (type_, EventType(..), preventDefault)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)
import Control.Monad.Reader (class MonadAsk, asks)

import Model.Route(Route(..))
import Capability.Navigate (class Navigate, navigate)
-- TODO - move this to some kind of supplemental library
inputR :: forall f a. (a -> f Unit) -> a -> Maybe (f Unit)
inputR f x = Just $ (f x)


-- TODO - result and error should just be captured as one Either value
type State =
  { loading :: Boolean
  , email :: String
  , password :: String
  , error :: Maybe String
  }

data Query a
  = SetEmail String a
  | SetPassword String a
  | Submit a
  | PreventDefault Event (Query a)
  | GoHome a

component :: forall m r
    . MonadAff m
   => Navigate m
   => MonadAsk { apiUrl :: String | r } m
   => H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, email: "", password: "", error: Nothing}

  -- TODO - break this up into smaller components
  render :: State -> H.ComponentHTML Query
  render st =
    HH.form_ $
      [ HH.a
        [ HP.href "#"
        , HE.onClick $ inputR \e ->
                       PreventDefault (toEvent e) $
                       H.action $ GoHome
        ]
        [ HH.text "Home" ]
      , HH.h1_ [ HH.text "Register" ]
      , HH.label_
          [ HH.div_ [ HH.text "email: " ]
          , HH.input
              [ HP.value st.email
              , HE.onValueInput (HE.input SetEmail)
              ]
          ]
      , HH.label_
          [ HH.div_ [ HH.text "Password:" ]
          , HH.input
              [ HP.value st.password
              , HE.onValueInput (HE.input SetPassword)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick $ inputR \e ->
                          PreventDefault (toEvent e) $
                          H.action $ Submit
          ]
          [ HH.text "submit" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_ $
          case st.error of
            Nothing -> []
            Just err -> [ HH.p_ [ HH.text (err) ] ]
      ]

  --  TODO: pull out pure parts into their own functions
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    SetEmail email next -> do
      H.modify_ (_ { email = email })
      pure next
    SetPassword password next -> do
      H.modify_ (_ { password = password })
      pure next
    Submit next -> do
    --TODO - move  the API call here + parsing to a service file
      email <- H.gets _.email
      password <- H.gets _.password
      apiUrl <- asks _.apiUrl
      -- H.liftEffect $ log $ "api url in use: " <> apiUrl
      -- let reqUrl = apiUrl <> "/stocks/" <> symbol
      -- H.modify_ (_ { loading = true })
      -- response <- H.liftAff $ AX.get AXRF.string (reqUrl)
      -- let rb = hush response.body
      -- let (q :: Maybe Quote) = hush <<< JSON.readJSON =<< rb
      -- let e = if isNothing q then Just "Invalid Symbol" else Nothing
      -- H.modify_ (_ { loading = false, result = q, error = e })
      pure next
      -- TODO Move this out to a helper lib
    PreventDefault e q -> do
      let (EventType t) = type_ e
      H.liftEffect $ preventDefault e
      H.liftEffect $ log $ show t <> " default navigation prevented"
      eval q
    GoHome next -> do
      navigate Home
      pure next