module Atomic.SubHeader where
  
import Prelude (discard)
import Halogen.HTML as HH
import Halogen.HelperLib as HL
import Atomic.SubHeader.Styles (subHeader')


subHeader :: forall p i. String -> (HH.HTML p i)
subHeader text 
  = HH.h2 
      [ HL.class_ subHeader' ] 
      [ HH.text text ]