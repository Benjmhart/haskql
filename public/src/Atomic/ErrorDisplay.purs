module Atomic.ErrorDisplay where

import Halogen.HTML as HH
import Halogen.HelperLib as HL
import Atomic.ErrorDisplay.Styles (errorDisplay')


errorDisplay :: forall p i. String -> (HH.HTML p i)
errorDisplay err 
  =  HH.p [ HL.class_ errorDisplay' ] [ HH.text err ] 

