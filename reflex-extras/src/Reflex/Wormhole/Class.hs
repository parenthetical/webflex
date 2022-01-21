{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DefaultSignatures #-}
module Reflex.Wormhole.Class where

import Reflex.Class
import Control.Monad.Trans
import Data.Semigroup (getFirst, First(First))

class Wormholed t m where
  wormhole :: Semigroup a => m (Event t a, Event t a -> m ())
  default wormhole :: forall m' a f.
                      (MonadTrans f, m ~ f m', Wormholed t m', Monad m', Semigroup a
                      , Monad m)
                   => m (Event t a, Event t a -> m ())
  wormhole = do
    (e,f) <- lift wormhole
    pure (e, lift . f)


-- | Wormhole which will only have one simultaneous occurrence at most. Arbitrarily picks an occurrence depending on implementation.
unsafeWormhole :: (Wormholed t m, Reflex t, Monad m) => m (Event t a, Event t a -> m ())
unsafeWormhole = do
  (e,f) <- wormhole
  pure (fmapCheap getFirst e, f . fmap First)
