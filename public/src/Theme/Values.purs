module Theme.Values where

import Prelude ((#), ($))
import CSS (CSS, Color)
import CSS.Size (rem, pct, px, Size, Rel, Abs)
import CSS.Color(hsl)
import CSS.Font (fontSize)
import CSS.Additional ((##))

-- typography
headerFontSize :: CSS
headerFontSize = fontSize $ 3 ## rem
subheaderFontSize :: CSS
subheaderFontSize = fontSize $ rem2
standardFontSize :: CSS
standardFontSize = fontSize $ rem1
buttonFontSize :: CSS
buttonFontSize = fontSize $ 1.25 # rem

-- common sizes
rem1 :: Size Rel
rem1 = 1 ## rem  
rem2 :: Size Rel
rem2 = 2 ## rem
qrem :: Size Rel
qrem = 0.25 # rem
hrem :: Size Rel
hrem = 0.5 # rem

full :: Size Rel
full = 100 ## pct 

px2:: Size Abs
px2 = 2 ## px
px5 :: Size Abs
px5 = 5 ## px


-- common colors

shadow :: Color
shadow = hsl 227.0 0.89 0.17

lightYellow :: Color
lightYellow = hsl 60.0 1.00 0.50

primary :: Color
primary = hsl 227.0 0.89 0.17

charcoal :: Color
charcoal = hsl 219.0 0.01 0.50

greyHighlight :: Color
greyHighlight = hsl 219.0 0.01 0.20

offWhite :: Color
offWhite = hsl 0.0 0.0 0.98

alertRed :: Color
alertRed = hsl 0.0 0.86 0.53 

