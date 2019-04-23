module Component.SymbolSearch (styles, State, Query(..), component) where

import Prelude (type (~>), map, Unit, Void, bind, const, discard, pure, ($), (<<<), (<>), (=<<))

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Bifunctor(lmap)
import Foreign(ForeignError, renderForeignError)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Options.Applicative.Internal.Utils
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HelperLib as HL
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Simple.JSON as JSON
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)
import Control.Monad.Reader (class MonadAsk, asks)
import Model.Quote (Quote)
import CSS (CSS) -- , height, width)
-- import CSS.Size (pct)
import Component.StockResult as Result

-- TODO move this to a helper library for Affjax stuff
transformError :: AXRF.ResponseFormatError -> NonEmptyList ForeignError
transformError (AXRF.ResponseFormatError e _) = singleton e

-- classNames
ss                  :: String
ss                  = "symbol-search"
ssHeader            :: String
ssHeader            = "symbol-search__header"
ssForm              :: String
ssForm              = "symbol-search__form"
ssSymbolField       :: String
ssSymbolField       = "symbol-search__form__symbol-field"
ssSymbolFieldLabel  :: String
ssSymbolFieldLabel  = "symbol-search__form__symbol-field__label"
ssSymbolFieldInput  :: String
ssSymbolFieldInput  = "symbol-search__form__symbol-field__input"
ssFetchButton       :: String
ssFetchButton       = "symbol-search__form__submit-button"
ssResults           :: String
ssResults           = "symbol-search__Results"
ssWorking           :: String
ssWorking           = "symbol-search__Working"
ssError             :: String
ssError             = "symbol-search__Error"

styles :: Array CSS
styles = 
  [

  ]

-- TODO - result and error should just be captured as one Either value
type State =
  { loading :: Boolean
  , symbol :: String
  , result :: Either String (Maybe Quote)
  }

data Query a
  = SetSymbol String a
  | MakeRequest a
  | PreventDefault Event (Query a)


component :: forall m r
    . MonadAff m
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
  initialState = { loading: false, symbol: "", result: Right Nothing}

  -- TODO - break this up into smaller components
  render :: State -> H.ComponentHTML Query
  render st =
    HH.div [ HL.class_ ss ]
      [ HH.h1 
        [ HL.class_ ssHeader ] 
        [ HH.text "Lookup Stock Quote" ]
      , HH.form 
          [ HL.class_ ssForm ] 
          [ HH.label
              [ HL.class_ ssSymbolField]
              [ HH.div
                [ HL.class_ ssSymbolFieldLabel ]
                [ HH.text "Stock Symbol: " ]
              , HH.input
                  [ HL.class_ ssSymbolFieldInput
                  , HP.value st.symbol
                  , HE.onValueInput (HE.input SetSymbol)
                  ]
              ]
          , HH.button
              [ HL.class_ ssFetchButton
              , HP.disabled st.loading
              , HE.onClick $ HL.inputR \e ->
                              PreventDefault (toEvent e) $
                              H.action $ MakeRequest
              ]
              [ HH.text "Fetch info" ]
          ]
      , HH.p
          [ HL.class_ ssWorking ]
          [ HH.text (if st.loading then "Working..." else "") ]
      , Result.component st.result
      ]


  --  TODO: pull out pure parts into their own functions
  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    SetSymbol symbol next -> do
      H.modify_ (_ { symbol = symbol
                   , result = Right Nothing
                   })
      pure next
    MakeRequest next -> do
    --TODO - move  the API call here + parsing to a monad capability file
      symbol <- H.gets _.symbol
      apiUrl <- asks _.apiUrl
      H.liftEffect $ log $ "api url in use: " <> apiUrl
      let reqUrl = apiUrl <> "/stocks/" <> symbol
      H.modify_ (_ { loading = true })
      response <- H.liftAff $ AX.get AXRF.string (reqUrl)
      let rb = lmap transformError $ response.body
      let result = lmap (unLines <<< (map renderForeignError) ) $ (JSON.readJSON =<< rb)
      H.modify_ (_ { loading = false, result = result })
      pure next
      -- TODO Move this out to a helper lib
    PreventDefault e q -> do
      H.liftEffect $ HL.preventDefault e
      eval q

