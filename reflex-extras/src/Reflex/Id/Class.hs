{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
module Reflex.Id.Class where

import Reflex
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

class GetId (m :: * -> *) where
  type Id m :: *
  getId :: m (Id m)
  default getId :: (MonadTrans f, GetId m', Monad m', m ~ f m', Id m ~ Id m') => m (Id m)
  getId = lift getId

instance (Monad m, GetId m) => GetId (StateT s m) where
  type Id (StateT s m) = Id m

instance (Monad m, GetId m) => GetId (EventWriterT t w m) where
  type Id (EventWriterT t w m) = Id m

instance (Monad m, GetId m) => GetId (DynamicWriterT t w m) where
  type Id (DynamicWriterT t w m) = Id m

instance (Monad m, GetId m) => GetId (ReaderT r m) where
  type Id (ReaderT r m) = Id m

instance (Monad m, GetId m) => GetId (ExceptT r m) where
  type Id (ExceptT r m) = Id m
