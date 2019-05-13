module Page.SymbolSearch ( State, Query(..), component) where

import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($), (<<<), (<>), (=<<), (<$>), join)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Bifunctor(lmap)
import Foreign(ForeignError)
import Data.List.NonEmpty (NonEmptyList, singleton)
import Effect.Aff(try)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
-- import Effect.Exception (error, message)
-- import Options.Applicative.Internal.Utils
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
    HH.div_
      [ subHeader "Lookup Stock Quote" 
      , HH.form 
          [ HL.class_ ssForm ] 
          [ field_
              [ fieldLabel_
                [ HH.text "Stock Symbol: " ]
              , textField
                  [ HP.value st.symbol
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
      H.modify_ (_ { loading = true, result = (Right Nothing) })
      response <- H.liftAff $ try $ AX.get AXRF.string (reqUrl)
      let nested = (networkErrorString $ _.body <$> response) :: Either String (Either AXRF.ResponseFormatError String)
      let rb = (join $ responseErrorToString <$> nested) :: Either String String
      let parsed = safeParseJSON =<< rb
      H.modify_ (_ { loading = false, result = parsed })
      pure next
    PreventDefault e q -> do
      H.liftEffect $ HL.preventDefault e
      eval q
    where
      -- message from Effect.Exception to get network error as string
      networkErrorString = (lmap (\_ -> "Network error")) 
      responseErrorToString = lmap AXRF.printResponseFormatError
      -- (unLines <<< (map renderForeignError) to lmap and get errors
      safeParseJSON = lmap (\_ -> "Invalid Symbol") <<< JSON.readJSON

