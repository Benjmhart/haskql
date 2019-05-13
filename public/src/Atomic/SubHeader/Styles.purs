module Atomic.SubHeader.Styles where

import Prelude (discard)
import CSS (CSS, color)
import CSS.Additional ((??))
import Theme.Values ( primary, subheaderFontSize)

subHeader' :: String
subHeader' = "sub-header"

styles :: Array CSS
styles =  [ subHeader' ?? do
              subheaderFontSize
              color primary
          ]