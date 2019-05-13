module Atomic.FieldLabel.Styles where

import Prelude (discard)
import CSS (CSS, color)
import CSS.Additional ((??))
import Theme.Values (primary, standardFontSize)

fieldLabel' :: String
fieldLabel' = "form-field-label"

styles :: Array CSS
styles =  [ fieldLabel' ?? do
              color primary
              standardFontSize
          ]