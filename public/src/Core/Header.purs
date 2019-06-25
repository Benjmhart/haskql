module Core.Header where 

import Prelude (type (~>), Unit, Void, discard, identity, pure, ($))
import Capability.Navigate (class Navigate, navigate)
import Capability.Log (class Log)
import Effect.Aff.Class (class MonadAff)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HelperLib as HL
import Core.Header.Styles (header, titleBar, brand, nav, navItem, navItemLink)
import Model.Route (Route(..))
import Data.Const (Const)
import Data.Maybe (Maybe(..))

type State = Route 

data Action
  = SetRoute Route 
  | GoRegister 
  | GoHome 
  | PreventDefault Event (Action) -- GoLogin, etc

type Input = Route

component
  :: forall m 
   . MonadAff m
  => Log m
  => Navigate m
  => H.Component HH.HTML (Const Void) Route Void m
component = 
    H.mkComponent
    { initialState: identity 
    , render
    , eval: H.mkEval $ H.defaultEval { 
      handleAction = handleAction 
      }  -- the r is implicit through pointfree
    }
  where 
    -- TODO: Case match over route to hide irrelevant navigation options
    render :: State -> H.ComponentHTML Action () m
    render r =
      HH.div
        [ HL.class_ header]
        [ HH.div
            [ HL.class_ titleBar ]
            [ HH.h1
                [ HL.class_ brand ]
                [ HH.text "📈 Hastock"]
            ]
        , HH.nav 
            [ HL.class_ nav ] 
            [ HH.div 
                [HL.class_ navItem] 
                [ HH.a
                  [ HL.class_ navItemLink
                  , HP.href "#"
                  , HE.onClick $ (\e -> Just $
                                    PreventDefault (toEvent e) $
                                    GoHome
                                 )
                  ]
                  [ HH.text "Home" ]
                ]
            , HH.div 
                [HL.class_ navItem] 
                [ HH.a
                  [ HL.class_ navItemLink
                  , HP.href "#"
                  , HE.onClick $ (\e -> Just $
                                  PreventDefault (toEvent e) $
                                  GoRegister
                                 )
                  ]
                  [ HH.text "Register" ]
                ]
            ]
        ]
    handleAction :: Action -> H.HalogenM State Action () Void m Unit
    handleAction = case _ of
      SetRoute r -> do
        H.put r
      GoRegister -> do
        navigate Register
      GoHome -> do
        navigate Home
      PreventDefault e q -> do
        H.liftEffect $ HL.preventDefault e
        handleAction q