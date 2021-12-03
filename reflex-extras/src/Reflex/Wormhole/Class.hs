{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module Reflex.Wormhole.Class where

import Reflex.Class
import Control.Monad.Trans

class Wormholed t m where
  wormhole :: Semigroup a => m (Event t a, Event t a -> m ())
  default wormhole :: forall m' a f.
                      (MonadTrans f, m ~ f m', Wormholed t m', Monad m', Semigroup a
                      , Monad m)
                   => m (Event t a, Event t a -> m ())
  wormhole = do
    (e,f) <- lift wormhole
    pure (e, lift . f)

