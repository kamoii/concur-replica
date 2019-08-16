{-# LANGUAGE RecordWildCards #-}
module Concur.Replica.Run
  ( Config(..)
  , mkDefaultConfig
  , mkDefaultConfig'
  , run
  , runDefault
  ) where

import qualified Colog                           as Co
import qualified Torsor                          as Tr
import qualified Chronos                         as Ch
import           Concur.Core                     (SuspendF(StepView, StepIO, StepBlock, Forever), Widget, display, orr, step)

import           Control.Monad.Free              (Free(Pure, Free))

import qualified Data.ByteString.Lazy            as B
import qualified Data.ByteString.Char8           as BC
import           Data.Aeson                      ((.:), (.=))
import qualified Data.Aeson                      as A
import qualified Data.Map                        as M
import           Data.Maybe                      (fromMaybe, mapMaybe)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import           Unsafe.Coerce                   (unsafeCoerce)

import qualified Replica.VDOM                    as V
import           Replica.VDOM                    (fireEvent, defaultIndex)
import           Replica.VDOM.Types              (DOMEvent(DOMEvent), HTML)

import           Network.WebSockets.Connection   (ConnectionOptions, defaultConnectionOptions)
import           Network.Wai                     (Middleware)
import qualified Network.Wai.Handler.Replica     as R
import qualified Network.Wai.Handler.Warp        as W
import qualified Replica.Run.Log                 as R
import qualified Replica.Run.Types               as R

import           Debug.Trace

stepWidget :: Free (SuspendF HTML) a -> IO (Maybe (HTML, (Free (SuspendF HTML) a), R.Event -> Maybe (IO ())))
stepWidget v = case v of
  Pure a                   -> pure Nothing
  Free (StepView new next) -> pure $ Just (new, next, \event -> fireEvent new (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event))
  Free (StepIO io next)    -> io >>= stepWidget . next
  Free (StepBlock io next) -> io >>= stepWidget . next
  Free Forever             -> pure Nothing

data Config res = Config
  { cfgPort                    :: Int
  , cfgTitle                   :: T.Text
  , cfgHeader                  :: HTML
  , cfgWSConnectionOptions     :: ConnectionOptions
  , cfgMiddleware              :: Middleware
  , cfgLogAction               :: Co.LogAction IO R.Log
  , cfgWSInitialConnectLimit   :: Ch.Timespan      -- ^ Time limit for first connect
  , cfgWSReconnectionSpanLimit :: Ch.Timespan      -- ^ limit for re-connecting span
  , cfgResourceAquire          :: IO res
  , cfgResourceRelease         :: res -> IO ()
  }

mkDefaultConfig'
  :: Int
  -> T.Text
  -> IO res
  -> (res -> IO ())
  -> Config res
mkDefaultConfig' port title resourceAquire resourceRelease =
  Config
  { cfgPort                    = port
  , cfgTitle                   = title
  , cfgHeader                  = mempty
  , cfgWSConnectionOptions     = defaultConnectionOptions
  , cfgMiddleware              = id
  , cfgLogAction               = Co.cmap R.rlogToText Co.logTextStdout
  , cfgWSInitialConnectLimit   = 15 `Tr.scale` Ch.second
  , cfgWSReconnectionSpanLimit = 60 `Tr.scale` Ch.second
  , cfgResourceAquire          = resourceAquire
  , cfgResourceRelease         = resourceRelease
  }

mkDefaultConfig
  :: Int
  -> T.Text
  -> Config ()
mkDefaultConfig port title =
  mkDefaultConfig' port title (pure ()) (const $ pure ())

run :: Config res -> (res -> Widget HTML a) -> IO ()
run cfg initial = R.app (rcfg cfg) (W.run (cfgPort cfg))
  where
    rcfg Config{..} = R.Config
      { R.cfgTitle                    = cfgTitle
      , R.cfgHeader                   = cfgHeader
      , R.cfgWSConnectionOptions      = cfgWSConnectionOptions
      , R.cfgMiddleware               = cfgMiddleware
      , R.cfgLogAction                = cfgLogAction
      , R.cfgWSInitialConnectLimit    = cfgWSInitialConnectLimit
      , R.cfgWSReconnectionSpanLimit  = cfgWSReconnectionSpanLimit
      , R.cfgResourceAquire           = cfgResourceAquire
      , R.cfgResourceRelease          = cfgResourceRelease
      , R.cfgInitial                  = step . initial
      , R.cfgStep                     = stepWidget
      }

runDefault
  :: Int
  -> T.Text
  -> Widget HTML a
  -> IO ()
runDefault port title initial' =
  run (mkDefaultConfig port title) (const initial')
