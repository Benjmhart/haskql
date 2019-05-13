module Model.AppEnv where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
-- import Effect.Ref (Ref)
import Data.Maybe (Maybe(..))
-- import Data.Either (Either(..))
import Control.Monad.Reader.Trans (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Effect.Exception (Error)
import Type.Equality (class TypeEquals, from)
import Control.Monad.Error.Class(class MonadThrow, class MonadError)
import Capability.Now (class Now)
import Capability.LogMessages (class LogMessages)
import Capability.Navigate (class Navigate, navigate)
import Effect.Now as Now
import Effect.Console as Console
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Model.Log as Log
import Model.Route as Route
import Api.Request as Request
import Halogen.Router (pushRoute)

type AppEnv =  
  { logLevel    :: LogLevel
  , apiUrl      :: ApiUrl
  , baseUrl     :: BaseUrl
  , currentUser :: Ref (Maybe String) -- TODO make a User Type
  } 

-- type User = {}
--TODO - make these newTypes or DataTypes
type LogLevel = String

type ApiUrl = String

type BaseUrl = String

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

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do 
    env <- ask
    liftEffect case Log.reason log of
      Log.Debug -> pure unit
      _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< pushRoute 

  logout = do
    liftEffect <<< Ref.write Nothing =<< asks _.currentUser
    liftEffect Request.removeToken 
    navigate Route.Home
