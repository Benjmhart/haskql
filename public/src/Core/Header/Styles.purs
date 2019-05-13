module Core.Header.Styles where

import Prelude (discard)
import CSS (CSS, color, display, marginTop, nil)
import CSS.Display(flex)
import CSS.Flexbox(justifyContent, flexStart)
import CSS.Additional((??), margin2)

import Theme.Values (rem2, primary, headerFontSize)

-- Class names
header :: String
header = "header"
titleBar :: String
titleBar = "header__title-bar"
brand :: String
brand = "header__title-bar__brand"
nav :: String
nav = "header__nav"
navItem :: String
navItem = "header__nav__item"
navItemLink :: String
navItemLink = "header__nav__item__link"



styles :: Array CSS
styles = 
  [ brand ?? do
      color primary
      marginTop nil
      headerFontSize
  , nav ?? do
      display flex
      justifyContent flexStart
  , navItem ?? do
      color primary
      margin2 nil rem2
  ]