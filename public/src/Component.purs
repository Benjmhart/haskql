module Component (State, Query(..), ui) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class(liftEffect)
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


type State =
  { loading :: Boolean
  , symbol :: String
  , result :: Maybe String
  }

data Query a
  = SetSymbol String a
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
  initialState = { loading: false, symbol: "", result: Nothing }

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_ $
      [ HH.h1_ [ HH.text "Lookup Stock Quote" ]
      , HH.label_
          [ HH.div_ [ HH.text "Stock Symbol ::" ]
          , HH.input
              [ HP.value st.symbol
              , HE.onValueInput (HE.input SetSymbol)
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
    SetSymbol symbol next -> do
      H.modify_ (_ { symbol = symbol, result = Nothing :: Maybe String })
      pure next
    MakeRequest next -> do
      symbol <- H.gets _.symbol
      let reqUrl = "http://localhost:3000/stocks/" <> symbol
      liftEffect <<< log $ reqUrl
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.get AXRF.string ("http://localhost:3000/stocks/" <> symbol)
      liftEffect <<< log $ show $ response.status
      H.modify_ (_ { loading = false, result = hush response.body })
      pure next