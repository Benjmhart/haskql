-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

-- | Other imports
import Data.Aeson.Types (FromJSON, Value)
import Network.HTTP.Simple as NS

-- | Type synonym for an application model
type Model = Int

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | NoOp
  | SayHelloWorld
  | FetchStocks
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = 0                    -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel AddOne m = noEff (m + 1)
updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp m = noEff m
updateModel FetchStocks m =  m <# do
  request <- parseRequest "http://localhost:3000/quote/MSFT" 
  putStrLn (show request) >> pure NoOp
  -- ((httpJSON request) :: FromJSON a => IO(Response a)) >> pure NoOp
  
updateModel SayHelloWorld m = m <# do
  putStrLn "Hello World" >> pure NoOp

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick FetchStocks ] [ text "Get Stock Data" ]
 ]

-- -- | Haskell language pragma
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards #-}

-- -- | Haskell module declaration
-- module Main where

-- -- | Miso framework import
-- import Miso
-- import Miso.String

-- -- | Type synonym for an application model
-- type Model = Int

-- -- | Sum type for application events
-- data Action
--   = AddOne
--   | SubtractOne
--   | NoOp
--   | SayHelloWorld
--   deriving (Show, Eq)

-- -- | Entry point for a miso application
-- main :: IO ()
-- main = startApp App {..}
--   where
--     initialAction = SayHelloWorld -- initial action to be executed on application load
--     model  = 0                    -- initial model
--     update = updateModel          -- update function
--     view   = viewModel            -- view function
--     events = defaultEvents        -- default delegated events
--     subs   = []                   -- empty subscription list
--     mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')

-- -- | Updates model, optionally introduces side effects
-- updateModel :: Action -> Model -> Effect Action Model
-- updateModel AddOne m = noEff (m + 1)
-- updateModel SubtractOne m = noEff (m - 1)
-- updateModel NoOp m = noEff m
-- updateModel SayHelloWorld m = m <# do
--   putStrLn "Hello World" >> pure NoOp

-- -- | Constructs a virtual DOM from a model
-- viewModel :: Model -> View Action
-- viewModel x = div_ [] [
--    button_ [ onClick AddOne ] [ text "+" ]
--  , text (ms x)
--  , button_ [ onClick SubtractOne ] [ text "-" ]
--  ]
