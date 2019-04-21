module Component.Header where --(State, Query(..), component, style) where

import Prelude (type (~>), Unit, Void, bind, const, discard, pure, show, ($), (<<<), (<>), (=<<))
import Capability.Navigate (class Navigate, navigate)
import Data.Array (concat)
import Data.Maybe (Maybe(..), isNothing )
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HCSS
import CSS as CSS
import CSS.Stylesheet (CSS, Rule(..), runS)
import CSS.Color as COLOR
import Control.Monad.Reader (class MonadAsk, asks)


import Model.Route(Route(..))
