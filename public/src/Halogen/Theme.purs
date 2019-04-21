module Halogen.Theme where
  
import Prelude (discard, ($), (<>))
import Data.NonEmpty (NonEmpty(..))
import Data.Foldable (foldl)
import Data.Int (toNumber)
import CSS as CSS
import CSS.Size (pct, nil)
import CSS.Font as FONT
import CSS.Property as CP
import CSS ((?), CSS, html, body, height, width, margin, fontFamily)
import CSS.AdditionalProperties (textRendering, webKitFontSmoothing)



theme :: CSS
theme = foldl (<>) baseStyle componentStyles

componentStyles :: Array CSS
componentStyles = []

fontStrings :: Array String
fontStrings = []

fontValues :: NonEmpty Array FONT.GenericFontFamily
fontValues = NonEmpty (FONT.GenericFontFamily $ CP.value "Roboto")
  [FONT.sansSerif]

baseStyle :: CSS
baseStyle = do
  html ? do
    height $ pct $ toNumber 100
  body ? do
    height $ pct $ toNumber 100
    width  $ pct $ toNumber 100
    margin nil nil nil nil
    fontFamily fontStrings fontValues
    textRendering "optimizeLegibility"
    webKitFontSmoothing "antialiased"
  
        
