module Capability.Resource.FetchQuote where

import Prelude ((<<<), class Monad)

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import Data.Maybe(Maybe)
import Data.Either (Either)
import Model.Quote (Quote)
import Model.StockSymbol



class Monad m <= FetchQuote m where
  fetchQuote :: StockSymbol -> m (Either String (Maybe Quote))

instance fetchQuoteHalogenM :: FetchQuote m => FetchQuote (HalogenM st act slots msg m) where
  fetchQuote = lift <<< fetchQuote
