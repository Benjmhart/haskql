module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import Data.Either (hush)
import Data.Maybe (Maybe(..))
-- import Effect.Ref (Ref) // 
import Effect.Console (log)
import Effect.Ref as Ref
import Model.Urls (BaseUrl(..), ApiUrl(..))
import Model.AppEnv ( AppEnv, runAppM)
import Halogen.Theme (theme)
import Halogen.Router (getRoute, navListen)
import Core.Router as Router
import Core.Style (mountStyles)



-- | Run the app.
main :: String -> String -> String -> Effect Unit
main logLevel apiUrl baseUrl = HA.runHalogenAff do
  body <- HA.awaitBody
  -- mutable reference used to store user profile if there is one
  currentUser <- H.liftEffect $ Ref.new (Nothing :: Maybe String)
  -- landing page of the user - in case they did not navigate to home route
  -- this gets handed off to the router once we have one in place
  initialRoute <- H.liftEffect $ getRoute
  -- TODO - read a token from local storage and see if the user is logged in here
  -- liftEffect readToken >>= traverse_ \token -> do
  --   let requestOptions = { endpoint: User, method: Get }
  --   res <- liftAff $ request $ defaultRequest baseUrl (Just token) requestOptions
  --   let u = decodeAt "user" =<< lmap printResponseFormatError res.body
  --   liftEffect $ Ref.write (hush u) currentUser
  --   pure unit
  let 
    appEnv :: AppEnv
    appEnv = { logLevel, apiUrl: (ApiUrl apiUrl), baseUrl: (BaseUrl baseUrl), currentUser }
    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM appEnv) Router.component
  --
  -- run ReaderT..build 
  halogenStyle <- mountStyles theme
  halogenIO <- runUI rootComponent (hush initialRoute) body
  
  void $ H.liftEffect $ navListen \new -> do
    case new of
      Nothing -> log "invalid route"
      Just a -> launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate a

  pure unit