module Model.StockSymbol where


import Data.Newtype (class Newtype)

newtype StockSymbol = StockSymbol String
derive instance newtypeStockSymbol :: Newtype StockSymbol _