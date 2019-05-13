module Atomic.PrimaryButton.Styles where

import Prelude (discard, (#), ($))
import CSS (CSS, backgroundColor, color, minHeight, textWhitespace, whitespaceNoWrap)
import CSS.Size (rem)
import CSS.Font (bold, fontWeight, bolder)
import CSS.Text.Transform (textTransform, capitalize)
import CSS.Additional (borderNone, borderRadius_, (?:), (??))
import Theme.Values (buttonFontSize, charcoal, greyHighlight, offWhite, px5)
      
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