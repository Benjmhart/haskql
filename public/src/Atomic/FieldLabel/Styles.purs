module Atomic.FieldLabel.Styles where

import Prelude (discard, (#), ($))
import CSS (CSS, backgroundColor, color, height, margin, minHeight, textWhitespace, whitespaceNoWrap, width)
import CSS.Border (solid, borderBottom)
import CSS.Size (rem, nil)
import CSS.Font (bold, fontWeight, bolder)
import CSS.Text.Transform (textTransform, capitalize)
import CSS.Additional (borderNone, borderRadius_, padding1, (##), (??))
import CSS.TextAlign (textAlign, center)
import Theme.Values (buttonFontSize, charcoal, hrem, lightYellow, offWhite, primary, px2, px5, rem1, shadow, standardFontSize, greyHighlight, alertRed)

fieldLabel' :: String
fieldLabel' = "form-field-label"

styles :: Array CSS
styles =  [ fieldLabel' ?? do
              color primary
              standardFontSize
          ]