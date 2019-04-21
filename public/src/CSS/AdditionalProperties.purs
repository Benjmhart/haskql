module CSS.AdditionalProperties where
  
import Prelude (($))
import CSS.Stylesheet (key)
import CSS.String (fromString)
import CSS (CSS)

textRendering :: String -> CSS
textRendering = key $ fromString "text-rendering"

webKitFontSmoothing ::String -> CSS
webKitFontSmoothing = key $ fromString "-webkit-font-smoothing"