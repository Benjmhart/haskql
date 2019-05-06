module Halogen.HelperLib where

import Prelude (Unit, ($), (<<<), (<>), flip)
import Data.Maybe (Maybe(..))
-- import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.Event.Event as WEE
import Web.Event.Internal.Types (Event)
import Effect (Effect)
import CSS.Size (Size)



class_ :: forall r i. String -> HH.IProp ( class :: String | r ) i
class_ = HP.class_ <<< HH.ClassName

inputR :: forall f a. (a -> f Unit) -> a -> Maybe (f Unit)
inputR f x = Just $ (f x)
  
preventDefault :: Event -> Effect Unit
preventDefault e = do
      let (WEE.EventType t) = WEE.type_ e
      WEE.preventDefault e
      