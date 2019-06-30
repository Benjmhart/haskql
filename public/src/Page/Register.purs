module Page.Register  where

import Prelude

-- import Data.Array (concat)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String.Utils as SU
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
import Capability.Resource.Register (class Register, register)

import Capability.Log (class Log, log)
import Atomic.PrimaryButton (primaryButton)
import Atomic.FieldLabel (fieldLabel_)
import Atomic.TextField (textField)
import Atomic.Field (field_)
import Atomic.LoadingDisplay (loadingDisplay)
import Atomic.SubHeader (subHeader)
import Atomic.ErrorDisplay (errorDisplay)
import Data.Const (Const)
import Page.Register.Styles (regForm, regLoginButtonWrapper)
import Model.Urls(ApiUrl)
import Model.UserPostBody(UserPostBody(..),validateUserPostBody)
import Model.Token(Token)


type State =
  { loading :: Boolean
  , name :: String
  , email :: String
  , password :: String
  , verifyPassword :: String
  , result :: Either String (Maybe Token)
  }

data Action
  = SetEmail String
  | SetName String
  | SetPassword String
  | SetVerifyPassword String
  | Submit
  | PreventDefault Event (Action)

component :: forall m r
    . MonadAff m
   => Navigate m
   => Log m
   => Register m
   => MonadAsk { apiUrl :: ApiUrl | r } m
   => H.Component HH.HTML (Const Void) Unit Void m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval  $ H.defaultEval 
      { handleAction = handleAction
      }
    }
  where

  initialState :: State
  initialState = { loading: false, name:"", email: "", password: "", verifyPassword: "", result: Right Nothing}

  render :: State -> H.ComponentHTML Action () m
  render st =
    HH.div_
        [ subHeader "Register"
        , HH.form 
            [HL.class_ regForm]
            -- TODO make the fields width adjustable
            [ field_
                [ fieldLabel_ [ HH.text "Username: " ]
                , textField
                    [ HP.value st.name
                    , HE.onValueInput (Just <<< SetName)
                    ]
                ]
            , field_
                [ fieldLabel_ [ HH.text "Email: " ]
                , textField
                    [ HP.value st.email
                    , HE.onValueInput (Just <<< SetEmail)
                    , HP.type_ HP.InputEmail
                    ]
                ]
            , field_
                [ fieldLabel_ [ HH.text "Password:" ]
                , textField
                    [ HP.value st.password
                    , HE.onValueInput (Just <<< SetPassword)
                    , HP.type_ HP.InputPassword
                    ]
                ]
            , field_
                [ fieldLabel_ [ HH.text "Verify Password:" ]
                , textField
                    [ HP.value st.verifyPassword
                    , HE.onValueInput (Just <<< SetVerifyPassword)
                    , HP.type_ HP.InputPassword
                    ]
                ]
            , HH.div
                [ HL.class_ regLoginButtonWrapper ] 
                [ primaryButton
                    [ HP.disabled st.loading
                    , HE.onClick $ (\e -> Just $
                                    PreventDefault (toEvent e) $
                                    Submit
                                   )
                    ]
                    [ HH.text "submit" ]
                ]
            ]
        , loadingDisplay st.loading
        , HH.div_ $
            case st.result of
              Right _ -> []
              Left err -> [ errorDisplay ("Error: " <> SU.filter (\c -> c /= "\"" ) err) ]
        ]

  --  TODO: pull out pure parts into their own functions
  handleAction :: Action -> H.HalogenM State Action () Void m Unit
  handleAction = case _ of
    SetEmail email -> do
      H.modify_ (_ { email = email })
    SetName name -> do
      H.modify_ (_ { name = name})
    SetPassword password-> do
      H.modify_ (_ { password = password })
    SetVerifyPassword verifyPassword-> do
      H.modify_ (_ { verifyPassword = verifyPassword })
    Submit -> do
    --TODO - move  the API call here + parsing to a capability file
      email <- H.gets _.email
      name <- H.gets _.name
      password <- H.gets _.password
      verifyPassword <- H.gets _.verifyPassword
      let userPostBody = UserPostBody { email, password, name }
      apiUrl <- asks _.apiUrl
      case validateUserPostBody userPostBody verifyPassword of
        Left a ->  do
          log $ a
          H.modify_ (_ { result =  Left a })
        Right a -> do
            H.modify_ (_ { loading = true, result = (Right Nothing) })
            parsed <- register userPostBody
            H.modify_ (_ { loading = false, result = parsed })
            -- set user in reader
            -- set token  in localstorage
            -- navigate home
      -- TODO Move this out to a helper lib
    PreventDefault e q -> do
      H.liftEffect $ HL.preventDefault e
      handleAction q

