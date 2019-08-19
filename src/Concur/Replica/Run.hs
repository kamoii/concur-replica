{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Concur.Replica.Run
  ( Config(..)
  , mkDefaultConfig
  , mkDefaultConfig'
  , run
  , runDefault
  ) where

import qualified Colog                           as Co
import           Colog                           ((<&))
import qualified Torsor                          as Tr
import qualified Chronos                         as Ch
import           Concur.Core                     (SuspendF(StepView, StepIO, StepBlock, Forever), Widget, display, orr, step)
import qualified Concur.Replica.Log              as L

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
import qualified Network.Wai.Middleware.Logging  as ML
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
  , cfgLogAction               :: Co.LogAction IO (Ch.Time, L.Log)
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
  , cfgLogAction               = Co.cmap L.format Co.logTextStdout
  , cfgWSInitialConnectLimit   = 15 `Tr.scale` Ch.second
  , cfgWSReconnectionSpanLimit = 5 `Tr.scale` Ch.minute
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
run cfg@Config{cfgPort, cfgLogAction} initial = R.app (rcfg cfg) $ \app -> do
  greetLog <& terminalLogo <> "\nListening port=" <> T.pack (show cfgPort)
  W.run cfgPort app
  where
    tagTime a = (,) <$> Ch.now <*> pure a
    greetLog = Co.cmap L.Greeting $ Co.cmapM tagTime cfgLogAction
    waiLog   = Co.cmap L.WaiLog $ Co.cmapM tagTime cfgLogAction

    -- Adding logging middleware in front.
    -- TODO: Access log appears after "Session create" ... which is meh
    -- TODO: WS access log doesn't apear since ws connection is processed before middlewares
    rcfg Config{..} = R.Config
      { R.cfgTitle                    = cfgTitle
      , R.cfgHeader                   = cfgHeader
      , R.cfgWSConnectionOptions      = cfgWSConnectionOptions
      , R.cfgMiddleware               = cfgMiddleware . ML.loggingApacheLike (waiLog<&)
      , R.cfgLogAction                = Co.cmap (fmap L.ReplicaLog) cfgLogAction
      , R.cfgWSInitialConnectLimit    = cfgWSInitialConnectLimit
      , R.cfgWSReconnectionSpanLimit  = cfgWSReconnectionSpanLimit
      , R.cfgResourceAquire           = cfgResourceAquire
      , R.cfgResourceRelease          = cfgResourceRelease
      , R.cfgInitial                  = step . initial
      , R.cfgStep                     = stepWidget
      }

    -- created by `figlet`
    terminalLogo = id
      $ "                                                           _ _\n"
      <> "        ___ ___  _ __   ___ _   _ _ __      _ __ ___ _ __ | (_) ___ __ _\n"
      <> "       / __/ _ \\| '_ \\ / __| | | | '__|____| '__/ _ \\ '_ \\| | |/ __/ _` |\n"
      <> "      | (_| (_) | | | | (__| |_| | | |_____| | |  __/ |_) | | | (_| (_| |\n"
      <> "       \\___\\___/|_| |_|\\___|\\__,_|_|       |_|  \\___| .__/|_|_|\\___\\__,_|\n"
      <> "                                                    |_|\n"


runDefault
  :: Int
  -> T.Text
  -> Widget HTML a
  -> IO ()
runDefault port title initial' =
  run (mkDefaultConfig port title) (const initial')
