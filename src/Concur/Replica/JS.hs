module Concur.Replica.JS where

import Concur.Core (MonadThrow (throwM))
import Control.Exception (Exception)
import Control.Monad (void)
import Data.Aeson (FromJSON, Result (Error, Success), ToJSON, Value, fromJSON)
import Data.Text (Text)

newtype JSCode = JSCode Text

data JSException
    = JSException Text
    | JSResultDecodingError String
    deriving (Show)

instance Exception JSException

-- Has the same semantics as liftIO in concur.
--
--  * Doesn't block the whole application.
--  * mempty view is displayed
--  * Propre cleanup when cancelled.
--
class MonadThrow m => MonadJS m

-- liftJS :: JSCode -> m ()

-- Has the same semantics as liftUnsafeBlockingJS in concur.
--
--  * Might freeze the whole application indefinatelly.
--  * If the functions raises exception, JSException will be throwned.
--  * JS code must be a function thats returns a json-serializeble value.
--    Shouldn't fork any async code, since there is now way to safely cancell it.
--
-- Example:
--
--   liftUnsafeBlockingJS $ JSCode "function() { ....; return val }"
--
class MonadThrow m => MonadUnsafeBlockiingJS m where
    liftUnsafeBlockingJS' :: JSCode -> m Value

liftUnsafeBlockingJS_ :: MonadUnsafeBlockiingJS m => JSCode -> m ()
liftUnsafeBlockingJS_ = void . liftUnsafeBlockingJS'

liftUnsafeBlockingJS ::
    (FromJSON v, MonadUnsafeBlockiingJS m) =>
    JSCode ->
    m v
liftUnsafeBlockingJS jscode = do
    value <- liftUnsafeBlockingJS' jscode
    case fromJSON value of
        Error str -> throwM $ JSResultDecodingError str
        Success a -> pure a
