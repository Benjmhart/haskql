module Component.Header where --(State, Query(..), component, style) where

import Prelude (type (~>), Void, discard, identity, pure, ($))
import Capability.Navigate (class Navigate, navigate)
import Capability.LogMessages (class LogMessages)
import Effect.Aff.Class (class MonadAff)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (toEvent)
-- import Data.Array (concat)
-- import Data.Maybe (Maybe(..), isNothing )
-- import Data.Either (hush)
-- import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
-- import Halogen.HTML.CSS as HCSS
import Halogen.HelperLib as HL
-- import CSS as CSS
import CSS.Stylesheet (CSS) --, Rule(..), runS)
-- import CSS.Color as COLOR
-- import Control.Monad.Reader (class MonadAsk, asks)

import CSS (CSS, height, width, display, padding, margin)
import CSS.Display(flex)

import CSS.Flexbox(justifyContent, spaceAround)
import Model.Route(Route(..))



-- Class names
header :: String
header = "header"
headerItem :: String
headerItem = "header__item"


styles :: Array CSS
styles = [
  HL.select_ header $ (do
    display flex
    justifyContent spaceAround
  )
]

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
        [HL.class_ header] 
        [ HH.div 
            [HL.class_ headerItem] 
            [ HH.a
              [ HP.href "#"
              , HE.onClick $ HL.inputR \e ->
                            PreventDefault (toEvent e) $
                            H.action $ GoHome
              ]
              [ HH.text "Home" ]
            ]
        , HH.div 
            [HL.class_ headerItem] 
            [ HH.a
              [ HP.href "#"
              , HE.onClick $ HL.inputR \e ->
                            PreventDefault (toEvent e) $
                            H.action $ GoRegister
              ]
              [ HH.text "Register" ]
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