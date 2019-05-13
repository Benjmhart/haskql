module Atomic.FieldLabel where
  
import Prelude (($))
import Data.Array (concat)
import Halogen.HTML as HH
import Halogen.HelperLib as HL
import DOM.HTML.Indexed (HTMLdiv)
-- import CSS (CSS)
-- import CSS.Additional ( (??))
import Atomic.FieldLabel.Styles (fieldLabel')

-- TODO - add dynamic changes for placeholder and selected  states

fieldLabel :: forall p i. HH.Node HTMLdiv p i
fieldLabel properties children = HH.div iprops children
  where
    iprops = concat $  [ [ HL.class_ fieldLabel' ], properties ]

fieldLabel_ :: forall p i. Array (HH.HTML p i) -> HH.HTML p i
fieldLabel_ = fieldLabel []