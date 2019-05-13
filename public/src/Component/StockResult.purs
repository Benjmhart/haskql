module Component.StockResult where
  
import Prelude (($), (<>))

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
-- import Data.Array
import Halogen.HTML as HH

import Model.Quote (Quote)

component :: forall p i. Either String (Maybe Quote) -> (HH.HTML p i)
component st = 
      HH.div_ $
        case st of
          Left err -> [ HH.p_ [ HH.text (err) ] ]
          Right Nothing -> []
          Right (Just q) ->
            [ HH.h2_ [ HH.text $  q.symbol <> " Quote:" ]
            , HH.p_  [ HH.text $ "price: " <> q.price ]
            , HH.p_  [ HH.text $ "open: " <> q.open ]
            , HH.p_  [ HH.text $ "high: " <> q.high ]
            , HH.p_  [ HH.text $ "low: " <> q.low ]
            , HH.p_  [ HH.text $ "volume: " <> q.volume ]
            , HH.p_  [ HH.text $ "latest trading day: " <> q.latestTradingDay ]
            , HH.p_  [ HH.text $ "previous close: " <> q.previousClose ]
            , HH.p_  [ HH.text $ "change: " <> q.change ]
            , HH.p_  [ HH.text $ "change percent: " <> q.changePercent ]
            ]
      
