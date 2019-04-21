module Halogen.HelperLib where


import Prelude (($), (<>), Unit, unit, discard, show)
import Data.Maybe (Maybe(..))
import Halogen as H
import Web.Event.Event as WEE
import Web.Event.Internal.Types (Event)
import Effect (Effect)

inputR :: forall f a. (a -> f Unit) -> a -> Maybe (f Unit)
inputR f x = Just $ (f x)
  
preventDefault :: Event -> Effect Unit
preventDefault e = do
      let (WEE.EventType t) = WEE.type_ e
      WEE.preventDefault e
      