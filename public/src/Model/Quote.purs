module Model.Quote where

-- TODO make a newType/dataType wrapper for Quote


type Quote  = { "symbol"              :: String
              , "open"                :: String
              , "high"                :: String
              , "low"                 :: String
              , "price"               :: String
              , "volume"              :: String
              , "latestTradingDay"    :: String
              , "previousClose"       :: String
              , "change"              :: String
              , "changePercent"       :: String
              }


