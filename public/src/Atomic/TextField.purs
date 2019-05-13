module Atomic.TextField where
  
import Prelude (($))
import Data.Array (concat)
import Halogen.HTML as HH
import Halogen.HelperLib as HL
import DOM.HTML.Indexed (HTMLinput)
import CSS (CSS)
import CSS.Additional ( (??))
import Atomic.TextField.Styles (textField')

-- TODO - add dynamic changes for placeholder and selected  states

textField :: forall p i. HH.Leaf HTMLinput p i
textField properties = HH.input iprops
  where
    iprops = concat $  [ [ HL.class_ textField' ], properties ]