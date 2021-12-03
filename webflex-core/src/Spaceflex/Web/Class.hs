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

type JSON a = (FromJSON a, ToJSON a)

-- TOOD: rename C m -> Client m?
class (Ord (C m)) => WebM c s m | m -> c, m -> s where
  -- | C ID
  type C m :: *
  -- | Client-side Reflex monad.
  type CM m :: * -> *
  -- | Server-side Reflex monad.
  type SM m :: * -> *
  -- | Perception of event occurrences at a specific client.
  atC :: (JSON a) => Event s (Map (C m) a) -> m (Event c a)
  -- | Perception of event occurrences at Server.
  atS :: (JSON a) => Event c a -> m (Event s (C m, a))
  -- | Connected to server.
  askConnected :: m (Dynamic c Bool)
  -- TODO: Incremental set?
  -- | Connected clients.
  askConnections :: m (Incremental s (PatchMap (C m) ()))
  -- | FIXME: Unsafe, should be wrapped in a Behavior c to prevent server-side evaluation of values only defined on client.
  liftC :: CM m a -> m a
  -- | FIXME: Unsafe, should be wrapped in a Behavior s to prevent client-side evaluation of values only defined on server.
  liftS :: SM m a -> m a

atAllC :: (WebM c s m, Reflex s, FromJSON a, ToJSON a, Monad m)
  => Event s a -> m (Event c a)
atAllC e = do
  cs <- fmap (current . fmap Map.keysSet . incrementalToDynamic) askConnections
  atC ((\cs' a -> Map.fromSet (const a) cs') <$> cs <@> e)

askNewConnections :: (Reflex s, WebM c s m, Functor m) => m (Event s (Set (C m)))
askNewConnections =
  fmap (fmap (Map.keysSet . unPatchMap)
        . updatedIncremental)
  $ askConnections
