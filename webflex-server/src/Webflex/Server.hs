{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Webflex.Server where

import Reflex
import qualified Network.WebSockets as WS
import Data.Text as T
import qualified Data.Text.IO        as T
import qualified Data.Text.Encoding        as T
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever)
import           Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class
import Reflex.Host.Class
import Control.Monad.Identity
import Data.IORef
import Control.Concurrent
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import qualified Data.Map.Monoidal as MMap
import Reflex.Spider.Internal (SpiderTimelineEnv,HasSpiderTimeline,spiderTimeline)
import Data.Aeson
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BSS
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set
import Webflex.Base
import Data.Patch.Map
import Control.Exception
type ClientId = Int

runServer ::
  forall t m x.
  ( HasSpiderTimeline x
  , m ~ SpiderHost x
  , t ~ SpiderTimeline x
  )
  => ServerT Int t m ()
  -> SpiderHost x ()
runServer (ServerT p) = do
  c :: Chan (Either (Map ClientId (Maybe ())) (ClientId, Map Int Value)) <- liftIO $ newChan
  -- Received a message or a client connection was added or deleted
  (receivedNewClientE :: Event t (Either (Map ClientId (Maybe ())) (ClientId, Map Int Value)), receivedNewClientTrigRef) <-
    newEventWithTriggerRef
    -- clientChangeE:: Just () for a new connection, Nothing for terminal a disconnect
  let (clientChangeE, receivedE) = fanEither receivedNewClientE
  theClients :: Incremental t (PatchMap ClientId ()) <-
    holdIncremental mempty (PatchMap <$> clientChangeE)
  toSendHnd <-
    subscribeEvent
    =<< fmap (fmap getMonoidalMap . snd) (evalREWST p (receivedE, theClients) 0)
  clientsRef <- liftIO $ newIORef (0 :: Int, mempty :: Map ClientId WS.Connection)

  liftIO . putStrLn $ "Server starting..."
  liftIO . void . forkIO $ do
    forever $ do
      rcv <- readChan c
      (_,connections) <- readIORef clientsRef
      toSend <- fmap (fmap encode . fromMaybe Map.empty)
                     . runSpiderHostForTimeline (fireEventRefAndRead receivedNewClientTrigRef rcv toSendHnd)
                     $ (spiderTimeline :: SpiderTimelineEnv x)
      forM_ (Map.toList toSend) $ \(i, msg) ->
        forM_ (Map.lookup i connections) $ \conn -> do
            T.putStrLn ("Sending to client "
                        <> T.pack (show i) <> ": "
                        <> T.decodeUtf8 (BSL.toStrict msg))
            WS.sendTextData conn msg

  liftIO $ WS.runServer "127.0.0.1" 9160 $ \pending -> do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (pure ()) $ do
      clientId <- atomicModifyIORef clientsRef
                   (\(n,cs) -> ((n + 1, Map.insert n conn cs), n))
      runSpiderHostForTimeline (fireEventRef receivedNewClientTrigRef (Left (Map.singleton clientId (Just ())))) spiderTimeline
      (forever $ do
        msg :: BSS.ByteString <- WS.receiveData conn
        T.putStrLn $ "Received at server from client "
          <> T.pack (show clientId) <> ": " <> T.pack (show msg)
        let x = fromMaybe (error "TODO: received bogus data")
                $ decode (BSL.fromStrict msg)
        writeChan c (Right (clientId,x)))
        `catches`
        [ Handler (\(e :: WS.ConnectionException) -> do
                     T.putStrLn $ "Disconnecting client " <> T.pack (show clientId) <> " because of " <> T.pack (show e)
                     runSpiderHostForTimeline (fireEventRef receivedNewClientTrigRef (Left (Map.singleton clientId Nothing))) spiderTimeline
                  )
          -- TODO: Other exceptions?
        ]
