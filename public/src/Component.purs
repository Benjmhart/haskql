module Component (State, Query(..), ui) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console
import Data.Either (hush)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Debug.Trace
import Web.Event.Event (Event)
import Web.Event.Event as Event

t' x = trace ( show x ) (\_ -> x)

type State =
  { loading :: Boolean
  , username :: String
  , result :: Maybe String
  }

data Query a
  = SetUsername String a
  | MakeRequest a

ui :: H.Component HH.HTML Query Unit Void Aff
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { loading: false, username: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.h1_ [ HH.text "Lookup GitHub user" ]
      , HH.label_
          [ HH.div_ [ HH.text "Enter username:" ]
          , HH.input
              [ HP.value st.username
              , HE.onValueInput (HE.input SetUsername)
              ]
          ]
      , HH.button
          [ HP.disabled st.loading
          , HE.onClick (HE.input_ MakeRequest)
          ]
          [ HH.text "Fetch info" ]
      , HH.p_
          [ HH.text (if st.loading then "Working..." else "") ]
      , HH.div_
          case st.result of
            Nothing -> []
            Just res ->
              [ HH.h2_
                  [ HH.text "Response:" ]
              , HH.pre_
                  [ HH.code_ [ HH.text res ] ]
              ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval = case _ of
    SetUsername username next -> do
      H.modify_ (_ { username = username, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      username <- H.gets _.username
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
      H.modify_ (_ { loading = false, result = hush response.body })
      pure next



    -- MakeRequest next -> do
    --   response <- t' <<< H.liftAff $ AX.get AXResponse.String ("http://localhost:3000/stocks/MSFT")
    --   pure next