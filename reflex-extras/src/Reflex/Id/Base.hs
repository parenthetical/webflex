{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
module Reflex.Id.Base where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Reflex.Dom.Core

import Control.Monad.Reader
import Control.Monad.State.Lazy
import Data.Functor.Misc (ComposeMaybe(..))
import Data.FastMutableIntMap (PatchIntMap(..))
import Data.Traversable
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Map (DMap)
import Reflex.Id.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Compose
import Reflex.Wormhole.Class
import Reflex.Extra.Orphans ()

newtype IdT (m :: * -> *) a = IdT { unIdT :: StateT Int (ReaderT (Int -> Idnt) m) a }
  deriving newtype (Functor, Applicative, Monad, MonadFix, PostBuild t,MonadIO)
  deriving (MonadTrans,MonadTransControl) via (ComposeT (StateT Int) (ReaderT (Int -> Idnt)))

runIdT' :: (Functor m) => IdT m a -> m a
runIdT' (IdT ma) = fmap fst $ runReaderT (runStateT ma 0) Start

runIdT :: IdT m a -> Int -> (Int -> Idnt) -> m (a, Int)
runIdT (IdT ma) i f = runReaderT (runStateT ma i) f

evalIdT :: (Functor m) => IdT m a -> Int -> (Int -> Idnt) -> m a
evalIdT m i f = fmap fst $ runIdT m i f

data Idnt = Start Int | Idnt ::: Int
  deriving (Show, Read, Eq, Ord)


instance (Monad m) => GetId (IdT m) where
  type Id (IdT m) = Idnt
  getId = IdT $ do
    i <- get
    modify (+ 1)
    asks ($ i)


instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (IdT m) where
  -- Not entirely sure about ma handling
  runWithReplace ma e = IdT $ do
    i <- unIdT getId
    c <- lift . lift $ current <$> count e
    lift . lift . runWithReplace (evalIdT ma 0 (i ::: 0 :::))
      . fmap (\m -> do 
                 t <- sample c
                 evalIdT m 0 (i ::: t :::))
      $ e
  traverseIntMapWithKeyWithAdjust f im e = IdT $ do
    i <- unIdT getId
    c <- lift . lift $ current <$> count e
    lift . lift $ traverseIntMapWithKeyWithAdjust (\k (n,v) -> do
                                        t <- sample c
                                        evalIdT (f k v) 0 (i ::: t ::: n :::))
      (snd . mapAccumL (\n v -> (n + 1, (n,) $ v)) 0 $ im)
      (numberPatchIntMap <$> e)
  -- FIXME: Also do the same numbering thing here? Is it even necessary?
  traverseDMapWithKeyWithAdjust f dm e = IdT $ do
    i <- unIdT getId
    c <- lift . lift $ current <$> count e
    lift . lift $
      traverseDMapWithKeyWithAdjust
      (\k (ComposeInt (n,v)) -> do
          t <- sample c
          evalIdT (f k v) 0 (i ::: t ::: n :::))
      (numberDMap dm)
      (numberPatchDMap <$> e)
  traverseDMapWithKeyWithAdjustWithMove _f _dm _e =
    -- TODO
    error "Not implemented"
    -- IdT $ do
    -- i <- unIdT getId
    -- c <- lift . lift $ current <$> count e
    -- lift . lift $ traverseDMapWithKeyWithAdjustWithMove (\k v -> do
    --                                     t <- sample c
    --                                     evalIdT (f k v) 0 (i ::: t :::))
    --   dm e

instance (NotReady t m) => NotReady t (IdT m)


-- TODO: is this correct? Doubtful and haven't verified properly.
instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (IdT m) where
  type DomBuilderSpace (IdT m) = DomBuilderSpace m
  textNode = lift . textNode
  commentNode = lift . commentNode
  element elementTag cfg (IdT child) = IdT $ do
    old <- get
    f <- lift ask
    (el_, (a, new)) <- lift . lift $ element elementTag cfg $ runReaderT (runStateT child old) f
    put new
    pure (el_,a)
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (IdT child) = IdT $ do
    old <- get
    f <- lift ask
    (el_, (a,new)) <- lift . lift $ selectElement cfg $ runReaderT (runStateT child old) f
    put new
    pure (el_,a)
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

numberPatchIntMap :: PatchIntMap v -> PatchIntMap (Int,v)
numberPatchIntMap =
  PatchIntMap
  . snd
  . mapAccumL (\n mv -> (n + 1, (n,) <$> mv)) 0
  . unPatchIntMap

newtype ComposeInt f a = ComposeInt { getComposeInt :: (Int, f a) }

numberDMap :: DMap k v -> DMap k (ComposeInt v)
numberDMap = snd . DMap.mapAccumLWithKey (\i _k v -> (i + 1, ComposeInt . (i,) $ v)) 0
numberPatchDMap :: PatchDMap k v -> PatchDMap k (ComposeInt v)
numberPatchDMap =
  PatchDMap
  . snd
  . DMap.mapAccumLWithKey (\i _k (ComposeMaybe mv) -> (i + 1, ComposeMaybe (ComposeInt . (i,) <$> mv))) 0
  . unPatchDMap


instance MonadSample t m => MonadSample t (IdT m) where
  sample = lift . sample
instance MonadHold t m => MonadHold t (IdT m) where
  buildDynamic m e = lift $ buildDynamic m e
  headE = lift . headE

instance (PerformEvent t m) => PerformEvent t (IdT m) where
  type Performable (IdT m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance (TriggerEvent t m) => TriggerEvent t (IdT m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete

instance (Monad m, Wormholed t m) => Wormholed t (IdT m) where
  wormhole = IdT $ do
    (e,f) <- lift . lift $ wormhole
    pure (e, IdT . lift . lift . f)
