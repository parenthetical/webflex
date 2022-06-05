{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Webflex.Class where

import Reflex
import Data.Aeson
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Witherable (catMaybes)
import Control.Monad (join)
import Data.Functor (void)

type JSON a = (FromJSON a, ToJSON a)

-- TOOD: rename C m -> Client m?
-- | Client-server web programming in Reflex. The @c@ and @s@ type
-- variables are the type of Reflex timelines of the clients and the
-- server. There are corresponding @CM@ and @SM@ contexts for clients
-- and server.
class (Ord (C m)) => WebM c s m | m -> c, m -> s where
  -- | Client identifiers.
  type C m :: *
  -- | Client-side Reflex monad.
  type CM m :: * -> *
  -- | Server-side Reflex monad.
  type SM m :: * -> *
  -- TODO: Consider making this Event c [a] (enabling "batch processing").
  -- | Takes a server-side event with client-tagged occurrences, and
  -- returns a client-side event. The client-side event will have an
  -- occurrence when the server-side occurrence tagged with that
  -- particular client's identifier arrives at the client.
  atCE :: (JSON a) => Event s (Map (C m) a) -> m (Event c a)
  -- TODO: Consider making this `Event (Map (C m) a)` (batch processing).
  -- | Knowledge of client-side event occurrences at the server.
  atSE :: (JSON a) => Event c a -> m (Event s (C m, a))
  -- | Connected to server.
  askConnected :: m (Dynamic c Bool)
  -- TODO: Incremental set?
  -- | Connected clients.
  askConnections :: m (Incremental s (PatchMap (C m) ()))
  -- | Make the client-side Reflex program available in the 'WebM' monad. The result is
  -- wrapped in a (Behavior c) so that client-values cannot leak into
  -- server-side contexts.
  liftC' :: CM m a -> m (Behavior c a)
  -- | Make the server-side Reflex program available in the 'WebM' monad. The result is
  -- wrapped in a (Behavior s) so that server-values cannot leak into
  -- client-side contexts.
  liftS' :: SM m a -> m (Behavior s a)

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
-- TODO: Does this always resend on a reconnect, even if the client has the value already?
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

-- | Take away a layer of 'Behavior'.
class BFlattenable t (f :: * -> *) where
  flattenB :: Behavior t (f a) -> f a

instance (Reflex t) => BFlattenable t (Event t) where
  flattenB = switch
instance (Reflex t) => BFlattenable t (Dynamic t) where
  flattenB d = unsafeDynamic (flattenB (fmap current d)) (switch (fmap updated d))
instance (Reflex t) => BFlattenable t (Behavior t) where
  flattenB = join

-- TODO: Add constraints to BFlattenable? (Incremental needs (Patch a))
-- instance (Reflex t) => BFlattenable t (Incremental t) where
--   flattenB bi =
--     unsafeBuildIncremental
--     (sample (currentIncremental =<< bi))
--     undefined

flattenBIncremental :: (Reflex t, Patch p) => Behavior t (Incremental t p) -> Incremental t p
flattenBIncremental bi =
  unsafeBuildIncremental
  (sample (currentIncremental =<< bi))
  (switch (fmap updatedIncremental bi))


-- | Lift a "server program" into the Webflex monad.
liftS :: (WebM c s m, Functor m, BFlattenable s f) => SM m (f a) -> m (f a)
liftS = fmap flattenB . liftS'

liftS_ :: (WebM c s m, Functor m) => SM m a -> m ()
liftS_ = void . liftS'

-- | Lift a "client program" into the Webflex monad.
liftC :: (WebM c s m, Functor m, BFlattenable c f) => CM m (f a) -> m (f a)
liftC = fmap flattenB . liftC'

liftC_ :: (WebM c s m, Functor m) => CM m a -> m ()
liftC_ = void . liftC'

