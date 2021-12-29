{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Spaceflex.Web.Class where

import Reflex
import Data.Aeson
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Witherable (catMaybes)

type JSON a = (FromJSON a, ToJSON a)

-- TOOD: rename C m -> Client m?
class (Ord (C m)) => WebM c s m | m -> c, m -> s where
  -- | C ID
  type C m :: *
  -- | Client-side Reflex monad.
  type CM m :: * -> *
  -- | Server-side Reflex monad.
  type SM m :: * -> *
  -- TODO: Consider making this Event c [a] (enabling "batch processing").
  -- | Perception of server event occurrences at a specific client.
  atCE :: (JSON a) => Event s (Map (C m) a) -> m (Event c a)
  -- TODO: Consider making this `Event (Map (C m) a)`.
  -- | Perception of client event occurrences at Server.
  atSE :: (JSON a) => Event c a -> m (Event s (C m, a))
  -- | Connected to server.
  askConnected :: m (Dynamic c Bool)
  -- TODO: Incremental set?
  -- | Connected clients.
  askConnections :: m (Incremental s (PatchMap (C m) ()))
  -- | FIXME: Unsafe, should be wrapped in a Behavior c to prevent server-side evaluation of values only defined on client.
  liftC :: CM m a -> m a
  -- | FIXME: Unsafe, should be wrapped in a Behavior s to prevent client-side evaluation of values only defined on server.
  liftS :: SM m a -> m a

atSE_ :: (Reflex s, WebM c s m, Functor m) => (JSON a) => Event c a -> m (Event s a)
atSE_ = fmap (fmap snd) . atSE

atAllCE :: (WebM c s m, Reflex s, FromJSON a, ToJSON a, Monad m)
  => Event s a -> m (Event c a)
atAllCE e = do
  cs <- fmap (current . fmap Map.keysSet . incrementalToDynamic) askConnections
  atCE ((\cs' a -> Map.fromSet (const a) cs') <$> cs <@> e)

askNewConnections :: (Reflex s, WebM c s m, Functor m) => m (Event s (Set (C m)))
askNewConnections =
  fmap (fmap (Map.keysSet . unPatchMap)
        . updatedIncremental)
  $ askConnections

-- TODO: Test whether this works correctly with "new value at the same instant as (re)connect of client"
atAllCDyn :: forall a c s m. (JSON a, WebM c s m, Reflex s, Reflex c, MonadHold c (CM m), Monad m) => a -> Dynamic s a -> m (Dynamic c a)
atAllCDyn init d = do
  conns <- askConnections
  let newCon :: Event s (Map (C m) a) =
        pushAlways (\p -> do
                       v <- sample (current d)
                       pure . (v <$) . catMaybes . unPatchMap $ p)
        $ updatedIncremental conns
  reconnectValCE <- atCE newCon
  newValCE <- atAllCE (updated d)
  liftC $ holdDyn init (leftmost [newValCE,reconnectValCE])
