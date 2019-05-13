module Page.Register (State, Query(..), component) where

import Prelude (type (~>), Unit, Void, bind, const, discard, pure, ($))

-- import Data.Array (concat)
import Data.Maybe (Maybe(..) )
-- import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
-- import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HelperLib as HL
-- import Affjax as AX
-- import Affjax.ResponseFormat as AXRF
-- import Simple.JSON as JSON
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)
import Control.Monad.Reader (class MonadAsk, asks)

-- import Model.Route(Route(..))
import Capability.Navigate (class Navigate)
import Atomic.PrimaryButton (primaryButton)
import Atomic.FieldLabel (fieldLabel_)
import Atomic.TextField (textField)
import Atomic.Field (field_)
import Atomic.LoadingDisplay (loadingDisplay)
import Atomic.SubHeader (subHeader)
import Atomic.ErrorDisplay (errorDisplay)
import Page.Register.Styles (regForm, regLoginButtonWrapper)


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

  render :: State -> H.ComponentHTML Query
  render st =
    HH.div_
      [ subHeader "Register"
      , HH.form 
          [HL.class_ regForm]
          -- TODO make the fields width adjustable
          [ field_
              [ fieldLabel_ [ HH.text "Email: " ]
              , textField
                  [ HP.value st.email
                  , HE.onValueInput (HE.input SetEmail)
                  ]
              ]
          , field_
              [ fieldLabel_ [ HH.text "Password:" ]
              , textField
                  [ HP.value st.password
                  , HE.onValueInput (HE.input SetPassword)
                  ]
              ]
          , HH.div
              [ HL.class_ regLoginButtonWrapper ] 
              [ primaryButton
                  [ HP.disabled st.loading
                  , HE.onClick $ HL.inputR \e ->
                                  PreventDefault (toEvent e) $
                                  H.action $ Submit
                  ]
                  [ HH.text "submit" ]
              ]
          , loadingDisplay st.loading
          , HH.div_ $
              case st.error of
                Nothing -> []
                Just err -> [ errorDisplay err ]
          ]
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
    --TODO - move  the API call here + parsing to a capability file
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
      H.liftEffect $ HL.preventDefault e
      eval q