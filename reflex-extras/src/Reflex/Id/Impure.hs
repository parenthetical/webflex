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
module Reflex.Id.Impure where

import Control.Monad.Fix
import Control.Monad.IO.Class

import Reflex.Dom.Core
import Control.Monad.Reader
import Reflex.Id.Class
import Control.Monad.Trans.Control
import Reflex.Extra.Orphans()
import Reflex.Wormhole.Class
import Data.IORef
import Reflex.Persist.Class

newtype IdT (m :: * -> *) a = IdT { unIdT :: ReaderT (IORef Int) m a }
  deriving newtype (Functor, Applicative, Monad, MonadFix, PostBuild t, NotReady t, TriggerEvent t, PerformEvent t, Persist t)
  deriving (MonadTrans,MonadTransControl) via ReaderT (IORef Int)
  deriving (MonadSample t, MonadHold t) via ReaderT (IORef Int) m

runIdT :: (MonadIO m) => IdT m a -> m a
runIdT (IdT ma) = do
  r <- liftIO $ newIORef 0
  runReaderT ma r
  
runIdT' :: () => IdT m a -> IORef Int -> m a
runIdT' (IdT ma) r = do
  runReaderT ma r

data Idnt = Start Int | Idnt ::: Int
  deriving (Show, Read, Eq, Ord)


instance (MonadIO m) => GetId (IdT m) where
  type Id (IdT m) = Int
  getId = IdT $ do
    r <- ask
    liftIO $ atomicModifyIORef' r (\n -> let n' = n + 1 in (n',n))
                                  

instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (IdT m) where
  runWithReplace (IdT ma) e = IdT $ runWithReplace ma (fmapCheap unIdT e)
  traverseIntMapWithKeyWithAdjust f im e =
    IdT $ traverseIntMapWithKeyWithAdjust (\k -> unIdT . f k) im e
  traverseDMapWithKeyWithAdjust f dm e =
    IdT $ traverseDMapWithKeyWithAdjust (\k -> unIdT . f k) dm e
  traverseDMapWithKeyWithAdjustWithMove f dm e =
    IdT $ traverseDMapWithKeyWithAdjustWithMove (\k -> unIdT . f k) dm e

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (IdT m) where
  type DomBuilderSpace (IdT m) = DomBuilderSpace m

instance (Monad m, Wormholed t m) => Wormholed t (IdT m) where
  wormhole = IdT $ do
    (e,f) <- lift $ wormhole
    pure (e, IdT . lift . f)
