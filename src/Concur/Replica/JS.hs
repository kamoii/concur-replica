module Concur.Replica.JS where

import Concur.Core (MonadThrow)
import Data.Text (Text)
import Data.Aeson (FromJSON)

newtype JSCode = JSCode Text

newtype JSExcetion = JSExcetion Text

-- Has the same semantics as liftIO in concur.
--
--  * Doesn't block the whole application.
--  * mempty view is displayed
--  * Propre cleanup when cancelled.
--
class MonadThrow m => MonadJS m where
    -- liftJS :: JSCode -> m ()

-- Has the same semantics as liftUnsafeBlockingJS in concur.
--
--  * Might freeze the whole application indefinatelly.
--  * If the functions raises exception, JSException will be throwned.
--
-- Example:
--    
--   liftUnsafeBlockingJS $ JSCode "function(arg) { ....; return val }"
--
class MonadThrow m => MonadUnsafeBlockiingJS m where
    liftUnsafeBlockingJS :: FromJSON a => JSCode -> m a
