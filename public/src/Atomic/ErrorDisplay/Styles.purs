module Atomic.ErrorDisplay.Styles where
  
import Prelude (discard)
import CSS (CSS, color)
import CSS.Additional ((??))
import Theme.Values (alertRed, standardFontSize)


errorDisplay' :: String
errorDisplay' = "error-display-text"

styles :: Array CSS
styles = [ 
  errorDisplay' ?? do
      color alertRed
      standardFontSize
]