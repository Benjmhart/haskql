module Halogen.Theme where
  
import Prelude (discard, ($), (<>))
import Data.Array(concat)
import Data.NonEmpty (NonEmpty(..))
import Data.Foldable (foldl)
import CSS.Size (pct, nil)
import CSS.Font as FONT
import CSS.Property as CP
import CSS ((?), CSS, html, body, height, width, margin, fontFamily)
import CSS.AdditionalProperties (textRendering, webKitFontSmoothing)
import Component.Router as Router
import Component.Header as Header
import CSS.Overflow as OF



theme :: CSS
theme = foldl (<>) baseStyle componentStyles

componentStyles :: Array CSS
componentStyles = concat 
  [ Router.styles
  , Header.styles
  ]

fontStrings :: Array String
fontStrings = []

fontValues :: NonEmpty Array FONT.GenericFontFamily
fontValues = NonEmpty (FONT.GenericFontFamily $ CP.value "Roboto")
  [FONT.sansSerif]

baseStyle :: CSS
baseStyle = do
  html ? do
    height $ pct 100.0
  body ? do
    height $ pct 100.0
    width  $ pct 100.0
    margin nil nil nil nil
    fontFamily fontStrings fontValues
    textRendering "optimizeLegibility"
    webKitFontSmoothing "antialiased"
    OF.overflow $ OF.hidden
  
        
