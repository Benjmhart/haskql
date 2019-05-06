module Halogen.Theme where
  
import Prelude ((<>))
import Data.Array(concat)
import Data.Foldable (foldl)
import CSS (CSS)
import Atomic.PrimaryButton.Styles as PB
import Atomic.TextField.Styles as TF
import Atomic.FieldLabel.Styles as FL
import Atomic.Field.Styles as F
import Atomic.LoadingDisplay.Styles as LD
import Atomic.ErrorDisplay.Styles as ED
import Atomic.StockResult.Styles as SR
import Atomic.SubHeader.Styles as SH
import Core.Router.Styles as Router
import Core.Header.Styles as Header
import Page.SymbolSearch.Styles as SS
import Page.Register.Styles as Register
import Theme.BaseStyle as BS



theme :: CSS
theme = foldl (<>) BS.style componentStyles

componentStyles :: Array CSS
componentStyles = concat 
  [ Router.styles
  , Header.styles
  , Register.styles
  , SS.styles
  , PB.styles
  , TF.styles
  , FL.styles
  , F.styles
  , LD.styles
  , ED.styles
  , SR.styles
  , SH.styles
  ]




  
        
