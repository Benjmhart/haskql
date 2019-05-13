module Atomic.TextField.Styles where

import Prelude (discard, (#), ($))
import CSS (CSS, backgroundColor, height, margin, width)
import CSS.Border (solid, borderBottom)
import CSS.Size (rem, nil)
import CSS.Additional (borderNone, padding1, (##), (??))
import CSS.TextAlign (textAlign, center)
import Theme.Values (hrem, lightYellow, px2, rem1, shadow, standardFontSize)

textField' :: String
textField' = "form-field-text"

styles :: Array CSS
styles =  [ textField' ?? do
              margin rem1 rem1 rem1 nil
              width $ 7 ## rem
              height $ 2.5 # rem
              padding1 hrem
              backgroundColor lightYellow
              borderNone
              standardFontSize
              textAlign center
              borderBottom solid px2 shadow
          ]