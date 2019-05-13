module Atomic.Field where

import Prelude (($))
import Data.Array (concat)
import Halogen.HTML as HH
import Halogen.HelperLib as HL
import DOM.HTML.Indexed (HTMLlabel)
import CSS (CSS)
import CSS.Additional ( (??))
import Atomic.Field.Styles (field')



field :: forall p i. HH.Node HTMLlabel p i
field properties children = HH.label iprops children
  where
    iprops = concat $  [ [ HL.class_ field' ], properties ]

field_ :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
field_ = field []