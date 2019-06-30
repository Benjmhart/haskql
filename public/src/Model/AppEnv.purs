module Model.AppEnv where

import Prelude (class Applicative, class Apply, class Bind, class Functor, class Monad, type (~>), bind, discard, join, pure, unit, ($), (<$>), (<<<), (<>), (=<<), show)

import Effect.Aff (Aff, try)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
-- import Effect.Ref (Ref)
import Data.Maybe (Maybe(..))
import Data.Bifunctor(lmap)
import Data.Newtype (unwrap)
import Data.Either(Either(..))
import Data.HTTP.Method(Method(..))
-- import Data.Either (Either(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Exception (Error)
import Simple.JSON as JSON
import Type.Equality (class TypeEquals, from)
import Control.Monad.Error.Class(class MonadThrow, class MonadError)
import Capability.Now (class Now)
import Capability.Log (class Log, log)
import Capability.Navigate (class Navigate, navigate)
import Capability.Resource.FetchQuote (class FetchQuote)
import Capability.Resource.Register (class Register)
import Halogen as H
import Effect.Now as Now
import Effect.Console as Console
import Data.Either(Either)
import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Affjax.RequestBody as AXRB
import Affjax.RequestHeader as AXRH
import Data.Argonaut.Core as J
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.MediaType as MT
import Api.Endpoint(Endpoint(..), endpointCodec)
import Routing.Duplex (print)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Model.Log as Log
import Model.Route as Route
import Api.Request as Request
import Halogen.Router (pushRoute)
import Model.Urls (ApiUrl, BaseUrl)
import Model.UserPostBody(UserPostBody(..))
import Model.Token(Token(..))
import Data.HTTP.Method (Method(..))


type AppEnv =  
  { logLevel    :: LogLevel
  , apiUrl      :: ApiUrl
  , baseUrl     :: BaseUrl
  , currentUser :: Ref (Maybe String) -- TODO make a User Type
  } 

-- type User = {}
--TODO - make these newTypes or DataTypes
type LogLevel = String


newtype AppM a = AppM (ReaderT AppEnv Aff a)

runAppM :: AppEnv -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM
derive newtype instance monadThrowAppM :: MonadThrow Error AppM
derive newtype instance monadErrorAppM :: MonadError Error AppM

instance monadAskAppM :: TypeEquals e AppEnv => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logAppM :: Log AppM where
  log a = liftEffect $ Console.log $ show a

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< pushRoute 

  logout = do
    liftEffect <<< Ref.write Nothing =<< asks _.currentUser
    liftEffect Request.removeToken 
    navigate Route.Home

instance fetchQuoteAppM :: FetchQuote AppM where
  fetchQuote symbol = do
      apiUrl <- asks _.apiUrl
      H.liftEffect $ Console.log $ "api url in use: " <> (unwrap apiUrl)
      let 
        ep = print endpointCodec $ FetchQuote (unwrap symbol)
        reqUrl = (unwrap apiUrl) <> ep
      response <- H.liftAff $ try $ AX.get AXRF.string (reqUrl)
      let 
        nested = (networkErrorString $ _.body <$> response) :: Either String (Either AXRF.ResponseFormatError String) 
        rb = (join $ responseErrorToString <$> nested) :: Either String String
        parsed = safeParseJSON =<< rb
      pure parsed
      where 
        safeParseJSON = lmap (\_ -> "Invalid Symbol") <<< JSON.readJSON

-- message from Effect.Exception to get network error as string
networkErrorString = (lmap (\_ -> "Network error")) 
responseErrorToString = lmap AXRF.printResponseFormatError
-- (unLines <<< (map renderForeignError) to lmap and get errors

instance registerAppM :: Register AppM where
  register userPostBody = do
      apiUrl <- asks _.apiUrl
      let 
        ep = print endpointCodec $ Register
        reqUrl = (unwrap apiUrl) <> ep
      log $ "api url in use: " <> (reqUrl)
      let  requestBody = AXRB.json $ encodeJson userPostBody
      -- TODO: factor the call out into its own typeclass

      response <- H.liftAff $ try $ AX.post AXRF.string (reqUrl) requestBody
      let 
        nested = (networkErrorString $ _.body <$> response) :: Either String (Either AXRF.ResponseFormatError String)
        rb = (join $ responseErrorToString <$> nested) :: Either String String
        parsed = safeParseJSON=<< rb
      log $ "response" <> show rb
      log $ "parsed" <> show parsed
      pure parsed
      where 
        safeParseJSON = lmap (\_ -> "Invalid Registration") <<< JSON.readJSON
