module Page.Register.Styles where
  
import Prelude (($), (#))
import CSS (CSS, display, paddingTop)
import CSS.Size (rem)
import CSS.Display(flex)
import CSS.Additional ((??))

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
  , regLoginButtonWrapper ?? do
      paddingTop $ 2.25 # rem
  ]