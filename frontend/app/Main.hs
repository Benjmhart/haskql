-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import Miso
import Miso.String

-- | Other imports
import Data.Aeson
import Data.Aeson.Types (FromJSON, Value)
import JavaScript.Web.XMLHttpRequest
import qualified Data.ByteString.Lazy as B
import Data.Either
import Debug.Trace

t' x = trace x x

-- | Type synonym for an application model

data Quote = Quote { symbol :: String
                   , changePercent :: String
                   , price :: String
                   , low :: String
                   , high :: String
                   } deriving (Eq, Show)

instance FromJSON Quote where
  parseJSON = withObject "Quote" $ \v -> Quote
      <$> v .: "symbol"
      <*> v .: "change percent"
      <*> v .: "price"
      <*> v .: "low"
      <*> v .: "high"


type Model = [Quote]

-- | Sum type for application events
data Action
  = NoOp
  | FetchQuote
  | SetQuote (Either String Quote)
  deriving (Show, Eq)



fetchQuote :: IO(Either String Quote)
fetchQuote = do 
  Just json <- contents <$> xhrByteString req
  putStrLn . show $ json
  pure $ eitherDecodeStrict json
  where
    req = Request { reqMethod = GET
                  , reqURI = "http://localhost:3000/quote/MSFT"
                  , reqLogin = Nothing
                  , reqHeaders = []
                  , reqWithCredentials = False
                  , reqData = NoData
                  }


-- | Entry point for a miso application
main :: IO ()
main = startApp App {..}
  where
    initialAction = NoOp -- initial action to be executed on application load
    model  = []             -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')


-- | Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel FetchQuote m =  m <# do 
  SetQuote <$> fetchQuote
updateModel (SetQuote result) m = 
  case result of
    Left str -> m <# do
      putStrLn str >> pure NoOp
    Right quote -> noEff [quote]


-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x = div_ [] [
   button_ [ onClick FetchQuote ] [ text "Get Stock Data" ]
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
