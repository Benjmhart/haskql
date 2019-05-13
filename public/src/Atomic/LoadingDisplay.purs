module Atomic.LoadingDisplay where


import Halogen.HTML as HH
import Halogen.HelperLib as HL
import Atomic.LoadingDisplay.Styles (loadingDisplay')

loadingDisplay :: forall p i. Boolean -> (HH.HTML p i)
loadingDisplay isLoading 
  = HH.p
      [ HL.class_ loadingDisplay' ]
      [ HH.text (if isLoading then "Working..." else "") ]

