module Atomic.PrimaryButton where

import Prelude (($))
import Data.Array (concat)
import Halogen.HTML as HH
import Halogen.HelperLib as HL
import DOM.HTML.Indexed (HTMLbutton)
import Atomic.PrimaryButton.Styles (primaryButton')

primaryButton :: forall p i. HH.Node HTMLbutton p i
primaryButton properties children = HH.button iprops children
  where
    iprops = concat $ [ [ HL.class_ primaryButton' ], properties ]