module Halogen.Router (getRoute, navListen, pushRoute) where

import Prelude

import Data.Either (Either(..), hush)
import Data.Maybe (Maybe)
import Effect (Effect)
import Routing.Duplex (print, parse)
import Routing.Duplex.Parser (RouteError)
import Model.Route (Route, routeCodec)


foreign import getCurrentRoute :: Effect String

foreign import onNavigate :: (String -> Effect Unit) -> Effect Unit

foreign import push :: String -> String -> Effect Unit

getRoute :: Effect (Either RouteError Route)
getRoute = parse routeCodec <$> getCurrentRoute

navListen :: (Maybe Route -> Effect Unit) -> Effect Unit
navListen f = onNavigate listenFunction
  where 
    listenFunction r = f $ hush $ parse routeCodec r 

-- if this turns into a library the empty string should be a variable
pushRoute :: Route -> Effect Unit
pushRoute = push "" <<< print routeCodec
