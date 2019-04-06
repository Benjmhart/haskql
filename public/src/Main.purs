module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.VDom.Driver (runUI)
import Component.Router as Router
import Routing.Hash (getHash, matchesWith)
import Routing.Duplex (parse)
import Halogen (liftEffect)

import Data.Either (hush)
import Data.Maybe (Maybe(..))
-- import Effect.Ref (Ref) // 
import Effect.Ref as Ref
import Data.Foldable (traverse_) -- used to mount style
import Data.Const (Const)
import Data.Newtype (unwrap)
import Web.DOM.ParentNode (QuerySelector(..)) -- for style to get the head element

import Model.Route (routeCodec)
import Model.AppEnv (AppEnv, runAppM)
import CSS as CSS
import CSS.Stylesheet (CSS)
-- import CSS.Color as COLOR
import CSS ((?))

styleComponent :: CSS -> forall m. H.Component HH.HTML (Const Void) Unit Void m
styleComponent css =
  H.component
    { initialState: const unit
    , render: const $ HCSS.stylesheet $ css
    , eval: absurd <<< unwrap
    , receiver: const Nothing
    }

style:: CSS
style = do
          CSS.body ? do
            CSS.backgroundColor CSS.blue

-- | Run the app.
main :: String -> String -> String -> Effect Unit
main logLevel apiUrl baseUrl = HA.runHalogenAff do
  body <- HA.awaitBody
  -- mutable reference used to store user profile if there is one
  currentUser <- H.liftEffect $ Ref.new (Nothing :: Maybe String)
  -- landing page of the user - in case they did not navigate to home route
  -- this gets handed off to the router once we have one in place
  initialHash <- H.liftEffect $ getHash
  -- TODO - read a token from local storage and see if the user is logged in here
  -- liftEffect readToken >>= traverse_ \token -> do
  --   let requestOptions = { endpoint: User, method: Get }
  --   res <- liftAff $ request $ defaultRequest baseUrl (Just token) requestOptions
  --   let u = decodeAt "user" =<< lmap printResponseFormatError res.body
  --   liftEffect $ Ref.write (hush u) currentUser
  --   pure unit
  let 
    appEnv :: AppEnv
    appEnv = { logLevel, apiUrl, baseUrl, currentUser }
    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM appEnv) Router.component
  --
  -- run ReaderT..build 
    initialRoute = hush $ parse routeCodec initialHash
  halogenStyle <- traverse_ (runUI (styleComponent style) unit) =<< HA.selectElement (QuerySelector "head")
  halogenIO <- runUI rootComponent initialRoute body
  
  void $ liftEffect $ matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      launchAff_ $ halogenIO.query $ H.action $ Router.Navigate new

  pure unit