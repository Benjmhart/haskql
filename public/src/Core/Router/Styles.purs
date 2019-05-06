module Core.Router.Styles where
  
import Prelude (discard, ($))
import CSS (CSS, height, width, display, backgroundColor)
import CSS.Size (rem)
import CSS.Display(flex)
import CSS.Flexbox(justifyContent, spaceAround)
import CSS.Box (boxShadow)
import CSS.Border (border, solid)
import CSS.Additional ((??), (##), margin1, padding1)
import Theme.Values (full, rem1, qrem, px2, shadow, offWhite)

-- class names
coreLayout :: String
coreLayout = "core-layout"

bodyContent :: String
bodyContent = "body-content"

styles :: Array CSS
styles = 
  [ coreLayout ?? do
      height full
      width full
      display flex
      backgroundColor offWhite
      justifyContent spaceAround
  , bodyContent ?? do
      width $ 30 ## rem 
      margin1 rem1
      padding1 rem1
      boxShadow qrem qrem qrem shadow
      border solid px2 shadow
  ]