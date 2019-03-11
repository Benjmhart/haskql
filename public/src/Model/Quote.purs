module Model.Quote where

-- TODO make a newType wrapper for Quote

import Data.Maybe (Maybe(..))

type QuoteResponse = { "Global Quote" :: Maybe QuoteWithNums }

type QuoteWithNums =  { "01. symbol"              :: String
                      , "02. open"                :: String
                      , "03. high"                :: String
                      , "04. low"                 :: String
                      , "05. price"               :: String
                      , "06. volume"              :: String
                      , "07. latest trading day"  :: String
                      , "08. previous close"      :: String
                      , "09. change"              :: String
                      , "10. change percent"      :: String
                      }

type QuoteRecord  = { "symbol"              :: String
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

newtype Quote = Quote QuoteRecord

-- TODO - map over object.entries to do this.
removeNums :: QuoteWithNums -> Quote
removeNums qwn = Quote
  { "symbol"           : qwn."01. symbol"
  , "open"             : qwn."02. open"              
  , "high"             : qwn."03. high"              
  , "low"              : qwn."04. low"               
  , "price"            : qwn."05. price"             
  , "volume"           : qwn."06. volume"            
  , "latestTradingDay" : qwn."07. latest trading day"
  , "previousClose"    : qwn."08. previous close"    
  , "change"           : qwn."09. change"            
  , "changePercent"    : qwn."10. change percent"    
  }

getQuoteWN :: QuoteResponse -> Maybe QuoteWithNums
getQuoteWN qr = qr."Global Quote"