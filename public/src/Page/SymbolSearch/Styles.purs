module Page.SymbolSearch.Styles where

import Prelude (($), (#))
import CSS (CSS, display, paddingTop)
import CSS.Size (rem)
import CSS.Display(flex)
import CSS.Additional ((??))

-- classNames
ssForm              :: String
ssForm              = "symbol-search__form"
ssFetchButtonWrapper :: String
ssFetchButtonWrapper = "symbol-search__form__button-wrapper"

styles :: Array CSS
styles = 
  [ ssForm ?? do
      display flex 
  , ssFetchButtonWrapper ?? do
      paddingTop $ 2.25 # rem
  ]