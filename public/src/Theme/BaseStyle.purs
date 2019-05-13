module Theme.BaseStyle where


import Prelude (discard, ($))
import Data.NonEmpty (NonEmpty(..))
import CSS ((?), CSS, html, body, a, height, width,  fontFamily, color)
import CSS.Property as CP
import CSS.Overflow (overflow, hidden)
import CSS.Size (nil)
import CSS.Font as FONT
import CSS.Additional(textRendering, webKitFontSmoothing, margin1)
import Theme.Values (full, primary)

fontStrings :: Array String
fontStrings = []

fontValues :: NonEmpty Array FONT.GenericFontFamily
fontValues = NonEmpty (FONT.GenericFontFamily $ CP.value "Roboto")
  [FONT.sansSerif]

style :: CSS
style = do
  html ? do
    height full
  body ? do
    height full
    width  full
    margin1 nil
    fontFamily fontStrings fontValues
    textRendering "optimizeLegibility"
    webKitFontSmoothing "antialiased"
    overflow hidden
  a ? do 
    color primary
