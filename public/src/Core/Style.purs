module Core.Style where
  


import Prelude (Unit, Void, absurd, const, unit, ($), (<<<), (=<<))
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Effect.Aff (Aff)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Const (Const)
import Web.DOM.ParentNode (QuerySelector(..)) -- for style to get the head element
import CSS.Stylesheet (CSS)
-- import CSS.Color as COLOR


mountStyles :: CSS -> Aff Unit
mountStyles css = traverse_ (runUI (styleComponent css) unit) =<< HA.selectElement (QuerySelector "head")

styleComponent :: CSS -> forall m. H.Component HH.HTML (Const Void) Unit Void m
styleComponent css =
  H.mkComponent
    { initialState: const unit
    , render: const $ HCSS.stylesheet $ css
    , eval: H.mkEval H.defaultEval
    }
         
