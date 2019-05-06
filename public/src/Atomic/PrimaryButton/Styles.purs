module Atomic.PrimaryButton.Styles where

import Prelude (discard, (#), ($))
import CSS (CSS, backgroundColor, color, height, margin, minHeight, textWhitespace, whitespaceNoWrap, width)
import CSS.Border (solid, borderBottom)
import CSS.Size (rem, nil)
import CSS.Font (bold, fontWeight, bolder)
import CSS.Text.Transform (textTransform, capitalize)
import CSS.Additional (borderNone, borderRadius_, padding1, (##), (??), (?:))
import CSS.TextAlign (textAlign, center)
import Theme.Values (buttonFontSize, charcoal, hrem, lightYellow, offWhite, primary, px2, px5, rem1, shadow, standardFontSize, greyHighlight, alertRed)
      
primaryButton' :: String
primaryButton' = "btn-primary"

styles :: Array CSS
styles =  [ primaryButton' ?? do
              minHeight $ 2.5 # rem 
              borderNone
              textWhitespace whitespaceNoWrap
              buttonFontSize
              fontWeight bold
              backgroundColor charcoal
              textTransform capitalize
              color offWhite
              borderRadius_ px5
          , primaryButton' ?: do
              fontWeight bolder
              backgroundColor greyHighlight
          ]