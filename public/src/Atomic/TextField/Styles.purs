module Atomic.TextField.Styles where

import Prelude (discard, (#), ($))
import CSS (CSS, backgroundColor, color, height, margin, minHeight, textWhitespace, whitespaceNoWrap, width)
import CSS.Border (solid, borderBottom)
import CSS.Size (rem, nil)
import CSS.Font (bold, fontWeight, bolder)
import CSS.Text.Transform (textTransform, capitalize)
import CSS.Additional (borderNone, borderRadius_, padding1, (##), (??))
import CSS.TextAlign (textAlign, center)
import Theme.Values (buttonFontSize, charcoal, hrem, lightYellow, offWhite, primary, px2, px5, rem1, shadow, standardFontSize, greyHighlight, alertRed)

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