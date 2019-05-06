module Core.Header where 

import Prelude (type (~>), Void, discard, identity, pure, ($))
import Capability.Navigate (class Navigate, navigate)
import Capability.LogMessages (class LogMessages)
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

type State = Route 

data Query a
  = SetRoute Route a 
  | GoRegister a 
  | GoHome a 
  | PreventDefault Event (Query a) -- GoLogin, etc

type Input = Route

component
  :: forall m 
   . MonadAff m
  => LogMessages m
  => Navigate m
  => H.Component HH.HTML Query Input Void m
component = 
    H.component
    { initialState: identity 
    , render
    , eval
    , receiver: HE.input SetRoute  -- the r is implicit through pointfree
    }
  where 
    -- TODO: Case match over route to hide irrelevant navigation options
    render :: State -> H.ComponentHTML Query
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
                  , HE.onClick $ HL.inputR \e ->
                                PreventDefault (toEvent e) $
                                H.action $ GoHome
                  ]
                  [ HH.text "Home" ]
                ]
            , HH.div 
                [HL.class_ navItem] 
                [ HH.a
                  [ HL.class_ navItemLink
                  , HP.href "#"
                  , HE.onClick $ HL.inputR \e ->
                                PreventDefault (toEvent e) $
                                H.action $ GoRegister
                  ]
                  [ HH.text "Register" ]
                ]
            ]
        ]
    eval :: Query ~> H.ComponentDSL State Query Void m
    eval = case _ of
      SetRoute r next -> do
        H.put r
        pure next
      GoRegister next -> do
        navigate Register
        pure next
      GoHome next -> do
        navigate Home
        pure next
      PreventDefault e q -> do
        H.liftEffect $ HL.preventDefault e
        eval q