module Page.Register.Styles where
  
import Prelude (($), (#), discard)
import CSS (CSS, display, paddingTop, width)
import CSS.Size (rem)
import CSS.Display(flex)
import CSS.Flexbox(flexWrap, wrap)
import CSS.Additional ((??), (##))

regForm :: String
regForm = "registration-form"
regHeader :: String
regHeader = "registration-form__header"
regLoginButtonWrapper :: String
regLoginButtonWrapper = "registration__login-button-wrapper"

styles :: Array CSS
styles = 
  [ regForm ?? do
      display flex 
      flexWrap wrap
      width $ 18 ## rem

  , regLoginButtonWrapper ?? do
      paddingTop $ 2.25 # rem
  ]