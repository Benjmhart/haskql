-- | A capability representing the ability to save status information to some output, which could 
-- | be the console, an external service like Splunk, or to the file system for testing purposes. 
-- | The implementation can be freely swapped out without changing any application code besides 
-- | the application monad, `Conduit.AppM`.
-- |
-- | To learn more about why we use capabilities and this architecture, please see the guide:
-- | https://thomashoneyman.com/guides/real-world-halogen/push-effects-to-the-edges/
module Capability.Log where

import Prelude

import Capability.Now (class Now)
import Model.Log (Log, LogReason(..), mkLog)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)

-- | We require a strict format for the messages that we log so that we can search this structured 
-- | data from our logging service later on. We can enforce this with the type system by using the 
-- | `Log` type from `Conduit.Data.Log`. This type can only be constructed using helper functions
-- | in that module, which enforce the correct format. However, we'll leave it up to the implementer
-- | to decide what to do with the log once created. 
class Monad m <= Log m where
  log :: forall a. Show a => a -> m Unit

-- | This instance lets us avoid having to use `lift` when we use these functions in a component.
instance logMessagesHalogenM :: Log m => Log (HalogenM s f g p o m) where
  log = lift <<< log

