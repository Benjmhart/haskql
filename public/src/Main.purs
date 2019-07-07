module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, try)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Control.Applicative(when)
import Data.Either (hush, Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Data.Traversable (traverse_)
import Data.Bifunctor (lmap)
-- import Effect.Ref (Ref) // 
import Effect.Console (log)
import Effect.Ref as Ref
import Model.Urls (BaseUrl(..), ApiUrl(..))
import Model.AppEnv ( AppEnv, runAppM, responseErrorToString, networkErrorString)
import Model.User (User(..))
import Model.LocalStorage(tokenKey, getLocalStorage)
import Halogen.Theme (theme)
import Halogen.Router (getRoute, navListen)
import Core.Router as Router
import Core.Style (mountStyles)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Affjax.StatusCode as AXSC
import Api.Endpoint(Endpoint(..), endpointCodec)
import Routing.Duplex (print)
import Data.HTTP.Method(Method(..))
import Simple.JSON as JSON


-- | Run the app.
main :: String -> String -> String -> Effect Unit
main logLevel apiUrl baseUrl = HA.runHalogenAff do
  body <- HA.awaitBody
  -- mutable reference used to store user profile if there is one
  currentUser <- H.liftEffect $ Ref.new (Nothing :: Maybe User)
  let
    ep = print endpointCodec $ GetUser
    reqUrl = apiUrl <> ep
  
  H.liftEffect $ log $ "request url in use " <> reqUrl
  H.liftEffect $ getLocalStorage tokenKey >>= traverse_ \tokenString -> do 
    log $ "token : " 
    log tokenString
    HA.runHalogenAff do
      res <- try $ AX.request AX.defaultRequest 
        { method = Left GET
        , url = reqUrl
        , headers = [ AXRH.RequestHeader "authorization" $ "Bearer " <> tokenString ]
        , responseFormat = AXRF.string
        }
      let 
        nested = (networkErrorString $ _.body <$> res) :: Either String (Either AXRF.ResponseFormatError String)
        status = (networkErrorString $ _.status <$> res) :: Either String (AXSC.StatusCode)
        rb = (join $ responseErrorToString <$> nested) :: Either String String
        safeParseJSON = lmap (\_ -> "Invalid Token") <<< JSON.readJSON
        parsed = (safeParseJSON =<< rb :: Either String { userResponseName :: String, userResponseToken :: String })
        u = (\p -> User { userName: p.userResponseName }) <$> parsed
        
      when (status == (Right (AXSC.StatusCode 200))) $ do
        H.liftEffect $ Ref.write (hush u) currentUser
        H.liftEffect $ log "it is done!"
  initialRoute <- H.liftEffect $ getRoute
  let
    appEnv :: AppEnv
    appEnv = { logLevel, apiUrl: (ApiUrl apiUrl), baseUrl: (BaseUrl baseUrl), currentUser }
    rootComponent :: H.Component HH.HTML Router.Query Router.Input Void Aff
    rootComponent = H.hoist (runAppM appEnv) Router.component
  halogenStyle <- mountStyles theme
  halogenIO <- runUI rootComponent (hush initialRoute) body
  
  void $ H.liftEffect $ navListen \new -> do
    case new of
      Nothing -> log "invalid route"
      Just a -> launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate a

  pure unit