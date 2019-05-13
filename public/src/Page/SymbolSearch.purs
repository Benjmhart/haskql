module Page.SymbolSearch ( State, Query(..), component) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Foreign(ForeignError)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Effect.Aff.Class (class MonadAff)
-- import Effect.Exception (error, message)
-- import Options.Applicative.Internal.Utils
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HelperLib as HL
import Capability.Resource.FetchQuote (class FetchQuote, fetchQuote)
import Effect.Console (log)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)
import Control.Monad.Reader (class MonadAsk, asks)
import Affjax.ResponseFormat as AXRF
import Model.Quote (Quote)
import Model.StockSymbol (StockSymbol(..))
import Model.Urls (ApiUrl)
import Atomic.StockResult (stockResult)
import Atomic.PrimaryButton (primaryButton)
import Atomic.FieldLabel (fieldLabel_)
import Atomic.TextField (textField)
import Atomic.Field (field_)
import Atomic.LoadingDisplay (loadingDisplay)
import Atomic.SubHeader (subHeader)
import Page.SymbolSearch.Styles (ssForm, ssFetchButtonWrapper)


-- TODO move this to a helper library for Affjax stuff
transformError :: AXRF.ResponseFormatError -> NonEmptyList ForeignError
transformError (AXRF.ResponseFormatError e _) = singleton e

type State =
  { loading :: Boolean
  , symbol :: StockSymbol
  , result :: Either String (Maybe Quote)
  }

data Query a
  = SetSymbol String a
  | MakeRequest a
  | PreventDefault Event (Query a)


component :: forall m r
    . MonadAff m
   => MonadAsk { apiUrl :: ApiUrl | r } m
   => FetchQuote m
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
  initialState = { loading: false, symbol: StockSymbol "", result: Right Nothing}

  -- TODO - break this up into smaller components
  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
      [ subHeader "Lookup Stock Quote" 
      , HH.form 
          [ HL.class_ ssForm ] 
          [ field_
              [ fieldLabel_
                [ HH.text "Stock Symbol: " ]
              , textField
                  [ HP.value $ unwrap st.symbol
                  , HE.onValueInput (HE.input SetSymbol)
                  ]
              ]
          , HH.div
              [ HL.class_ ssFetchButtonWrapper ]
              [ primaryButton
                  [ HP.disabled st.loading
                  , HE.onClick $ HL.inputR \e ->
                                  PreventDefault (toEvent e) $
                                  H.action $ MakeRequest
                  ]
                  [ HH.text "Fetch info" ]
              ]
          ]
      , loadingDisplay st.loading
      , stockResult st.result
      ]


  --  TODO: pull out pure parts into their own functions
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
  -- TODO: validate stock symbol and make a symbol type
    SetSymbol symbol next -> do
      H.modify_ (_ { symbol = StockSymbol symbol
                   , result = Right Nothing
                   })
      pure next
    MakeRequest next -> do
    --TODO - move  the API call here + parsing to a monad capability file
      symbol <- H.gets _.symbol
      apiUrl <- asks _.apiUrl
      case symbol of 
        (StockSymbol "") -> do
          H.liftEffect $ log $ "empty"
          H.modify_ (_ { result =  Left "You must enter a stock symbol" })
          pure next
        _                -> do 
          H.modify_ (_ { loading = true, result = (Right Nothing) })
          parsed <- fetchQuote symbol
          H.modify_ (_ { loading = false, result = parsed })
          pure next
    PreventDefault e q -> do
      H.liftEffect $ HL.preventDefault e
      eval q

