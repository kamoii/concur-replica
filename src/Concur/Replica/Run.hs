{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Concur.Replica.Run (
    Config (..),
    mkDefaultConfig,
    run,
    runDefault,
) where

import qualified Chronos as Ch
import Colog ((<&))
import qualified Colog as Co
import Concur.Core (ResourceT, SuspendF (Forever, StepBlock, StepIO, StepView), Widget (unWidget), display, orr)
import qualified Concur.Replica.Log as L
import qualified Torsor as Tr

import Control.Monad.Free (Free (Free, Pure))

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Unsafe.Coerce (unsafeCoerce)

import Replica.VDOM (defaultIndex, fireEvent)
import qualified Replica.VDOM as V
import Replica.VDOM.Types (DOMEvent (DOMEvent), HTML)

import qualified Network.HTTP.Types as H
import qualified Network.Wai as W
import qualified Network.Wai.Handler.Replica as R
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Logging as ML
import Network.WebSockets.Connection (ConnectionOptions, defaultConnectionOptions)
import qualified Replica.Run.Types as R

import Control.Monad.IO.Class (MonadIO (liftIO))

stepWidget ::
    Free (SuspendF HTML) a ->
    ResourceT IO (Maybe (HTML, Free (SuspendF HTML) a, R.Event -> Maybe (IO ())))
stepWidget v = case v of
    Pure a -> pure Nothing
    Free (StepView new next) -> pure $ Just (new, next, \event -> fireEvent new (R.evtPath event) (R.evtType event) (DOMEvent $ R.evtEvent event))
    Free (StepIO io) -> io >>= stepWidget
    Free (StepBlock _ io) -> liftIO io >>= stepWidget
    Free Forever -> pure Nothing

data Config = Config
    { cfgPort :: Int
    , cfgTitle :: T.Text
    , cfgHeader :: HTML
    , cfgWSConnectionOptions :: ConnectionOptions
    , cfgMiddleware :: W.Middleware
    , cfgLogAction :: Co.LogAction IO (Ch.Time, L.Log)
    , -- | Time limit for first connect
      cfgWSInitialConnectLimit :: Ch.Timespan
    , -- | limit for re-connecting span
      cfgWSReconnectionSpanLimit :: Ch.Timespan
    }

mkDefaultConfig ::
    Int ->
    T.Text ->
    Config
mkDefaultConfig port title =
    Config
        { cfgPort = port
        , cfgTitle = title
        , cfgHeader = mempty
        , cfgWSConnectionOptions = defaultConnectionOptions
        , cfgMiddleware = id
        , cfgLogAction = Co.cmap L.format Co.logTextStdout
        , cfgWSInitialConnectLimit = 15 `Tr.scale` Ch.second
        , cfgWSReconnectionSpanLimit = 5 `Tr.scale` Ch.minute
        }

run :: Config -> Widget HTML a -> IO ()
run cfg@Config{cfgPort, cfgLogAction} initial = R.app (rcfg cfg) $ \app -> do
    greetLog <& terminalLogo <> "\nListening port=" <> T.pack (show cfgPort)
    Warp.run cfgPort app
  where
    tagTime a = (,) <$> Ch.now <*> pure a
    greetLog = Co.cmap L.Greeting $ Co.cmapM tagTime cfgLogAction
    waiLog = Co.cmap L.WaiLog $ Co.cmapM tagTime cfgLogAction

    -- Adding logging middleware in front.
    -- TODO: Access log appears after "Session create" ... which is meh
    -- TODO: WS access log doesn't apear since ws connection is processed before middlewares
    rcfg Config{..} =
        R.Config
            { R.cfgTitle = cfgTitle
            , R.cfgHeader = cfgHeader
            , R.cfgWSConnectionOptions = cfgWSConnectionOptions
            , R.cfgMiddleware = ML.loggingApacheLike (waiLog <&) . cfgMiddleware
            , R.cfgLogAction = Co.cmap (fmap L.ReplicaLog) cfgLogAction
            , R.cfgWSInitialConnectLimit = cfgWSInitialConnectLimit
            , R.cfgWSReconnectionSpanLimit = cfgWSReconnectionSpanLimit
            , R.cfgInitial = pure $ unWidget initial
            , R.cfgStep = stepWidget
            }

    -- created by `figlet`
    terminalLogo =
        mconcat
            [ "                                                           _ _\n"
            , "        ___ ___  _ __   ___ _   _ _ __      _ __ ___ _ __ | (_) ___ __ _\n"
            , "       / __/ _ \\| '_ \\ / __| | | | '__|____| '__/ _ \\ '_ \\| | |/ __/ _` |\n"
            , "      | (_| (_) | | | | (__| |_| | | |_____| | |  __/ |_) | | | (_| (_| |\n"
            , "       \\___\\___/|_| |_|\\___|\\__,_|_|       |_|  \\___| .__/|_|_|\\___\\__,_|\n"
            , "                                                    |_|\n"
            ]

runDefault ::
    Int ->
    T.Text ->
    Widget HTML a ->
    IO ()
runDefault port title = run (mkDefaultConfig port title)
