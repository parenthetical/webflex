{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Reader-EventWriter-State transformer.
module Reflex.REWST where

import Reflex
import Control.Monad.State.Lazy
import Control.Monad.Reader


newtype REWST r w s t m a =
  REWST (StateT s (EventWriterT t w (ReaderT r m)) a)
  deriving ( Functor,Applicative,Monad,MonadFix,MonadState s,MonadReader r
           )
  
instance (Reflex t, Semigroup w, Monad m) => EventWriter t w (REWST r w s t m) where
  tellEvent = REWST . lift . tellEvent

instance MonadTrans (REWST r w s t) where
  lift = REWST . lift . lift . lift

evalREWST :: (Reflex t, Semigroup w, Monad m) => REWST r w s t m a -> r -> s -> m (a, Event t w)
evalREWST (REWST p) r s = do
  runReaderT (runEventWriterT (evalStateT p s)) r

