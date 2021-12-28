{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Webflex.Client where

import Reflex
import Reflex.Dom
import Data.Text as T
import qualified Data.Text.IO        as T
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever)
import           Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class
import Reflex.Host.Class
import Reflex.Dynamic
import Data.IORef
import Control.Concurrent
import Reflex.Spider.Internal (SpiderTimelineEnv,HasSpiderTimeline,spiderTimeline)
import Data.Bifunctor
import Reflex.Dom.WebSocket
import Data.Witherable (catMaybes)
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Void
import Spaceflex.Web.Base

-- transform received: (fmap (read . T.unpack) receivedE)
-- transform to send: fmap (T.pack . show) toSendE
clientWidget :: forall t m.
  ( Reflex t
  , MonadHold t m
  , PostBuild t m
  , DomBuilder t m
  , MonadFix m
  , MonadIO (Performable m)
  , MonadIO m
  , PerformEvent t m
  , TriggerEvent t m
  -- , MonadJSM m
  -- , MonadJSM (Performable m)
  -- , HasJSContext m
  , _
  )
  => ClientT t m () -> m ()
clientWidget (ClientT program) = mdo
  RawWebSocket receivedE socketOpenedE socketErrorE socketCloseE <-
    jsonWebSocket "ws://127.0.0.1:9160/"
     (WebSocketConfig
      (fmap (:[]) toSendE)
      never -- Close 
      True -- Reconnect
      [])
  socketOpen <- holdDyn False (leftmost [True <$ socketOpenedE, False <$ socketCloseE])
  performEvent_ ((liftIO . T.putStrLn $ "Socket opened") <$ socketOpenedE)
  performEvent_ ((liftIO . T.putStrLn $ "Socket error") <$ socketErrorE)
  performEvent_ (liftIO . T.putStrLn . ("Socket closed: " <>) . T.pack . show
                  <$> socketCloseE)
  performEvent_ (liftIO . T.putStrLn . ("Received at client: " <>) . T.pack . show
                  <$> receivedE)
  performEvent_ (liftIO . T.putStrLn . ("Sending to server: " <>) . T.pack . show
                  <$> toSendE)
  (_, toSendE) <- evalREWST program (catMaybes receivedE, socketOpen) 0 -- TODO: error on bogus data
  pure ()
