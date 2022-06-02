{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
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
import Reflex.Host.Headless
import Reflex.REWST

type ClientId = Int

serverTToHeadless :: forall t m. MonadHeadlessApp t m => ServerT Int t m () -> m (Event t ())
serverTToHeadless p = do
  (newClientE, newClientTrigger) <- newTriggerEvent
  (disconnectClientE, disconnectClientTrigger) <- newTriggerEvent
  (rcvE, rcvTrigger) <- newTriggerEvent
  let clientChangeE :: (Event t (Map ClientId (Maybe ()))) =
        leftmost [ (\c -> Map.singleton c (Just ())) <$> newClientE
                 , (\c -> Map.singleton c Nothing) <$> disconnectClientE
                 ]
  theClients :: Incremental t (PatchMap ClientId ()) <-
    holdIncremental mempty (PatchMap <$> clientChangeE)
  toSendE :: (Event t (Map ClientId (Map Int Value))) <-
    fmap (fmap getMonoidalMap . snd) (evalREWST (server p) (rcvE, theClients) 0)
  doSendMsgs <- liftIO $ wsServer newClientTrigger (curry rcvTrigger) disconnectClientTrigger
  performEvent_ (liftIO . doSendMsgs <$> toSendE)
  pure never

runServer :: (forall t m. MonadHeadlessApp t m => ServerT Int t m ()) -> IO ()
runServer x = runHeadlessApp (serverTToHeadless x)

wsServer :: (FromJSON a, ToJSON a)
         => (ClientId -> IO ())
         -> (ClientId -> a -> IO ())
         -> (ClientId -> IO ())
         -> IO (Map ClientId a -> IO ())
wsServer onNewClient onRcvMsg onDisconnectClient = do
  liftIO . putStrLn $ "Server starting..."
  clientsRef <- newIORef (0 :: Int, mempty :: Map ClientId WS.Connection)
  forkIO $ WS.runServer "127.0.0.1" 9160 $ \pending -> do
    conn <- WS.acceptRequest pending
    WS.withPingThread conn 30 (pure ()) $ do
      clientId <- atomicModifyIORef clientsRef
                   (\(n,cs) -> ((n + 1, Map.insert n conn cs), n))
      onNewClient clientId
      forever $ do
        msg :: BSS.ByteString <- WS.receiveData conn
        T.putStrLn $ "Received at server from client "
          <> T.pack (show clientId) <> ": " <> T.pack (show msg)
        let x = fromMaybe (error "TODO: received bogus data")
                $ decode (BSL.fromStrict msg)
        onRcvMsg clientId x
        `catches`
        [ Handler (\(e :: WS.ConnectionException) -> do
                     T.putStrLn $ "Disconnecting client " <> T.pack (show clientId) <> " because of " <> T.pack (show e)
                     onDisconnectClient clientId
                  )
          -- TODO: Other exceptions?
        ]
  pure $ \msgs -> do
    (_,connections) <- readIORef clientsRef
    let toSend = fmap encode msgs
    forM_ (Map.toList toSend) $ \(i, msg) ->
        forM_ (Map.lookup i connections) $ \conn -> do
            T.putStrLn ("Sending to client "
                        <> T.pack (show i) <> ": "
                        <> T.decodeUtf8 (BSL.toStrict msg))
            WS.sendTextData conn msg
