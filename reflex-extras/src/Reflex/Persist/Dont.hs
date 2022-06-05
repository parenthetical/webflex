{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Reflex.Persist.Dont where

import Reflex
import Reflex.Persist.Class
import Control.Monad.Fix
import Control.Monad.Trans
import Data.Coerce

newtype DontPersistT t m a = DontPersistT {runDontPersistT :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      NotReady t,
      DynamicWriter t w,
      EventWriter t w,
      MonadSample t,
      MonadHold t,
      PostBuild t,
      PerformEvent t,
      TriggerEvent t
    )

instance (Applicative m) => Persist t (DontPersistT t m) where
  persistE' _ = DontPersistT $ pure Nothing

instance (Adjustable t m) => Adjustable t (DontPersistT t m) where
  runWithReplace m e = DontPersistT $ runWithReplace (coerce m) (coerceEvent e)
  traverseIntMapWithKeyWithAdjust f dm0 dm = DontPersistT $ traverseIntMapWithKeyWithAdjust (coerce . f) (coerce dm0) (coerce dm)
  traverseDMapWithKeyWithAdjust f dm0 dm = DontPersistT $ traverseDMapWithKeyWithAdjust (coerce . f) (coerce dm0) (coerce dm)
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm = DontPersistT $ traverseDMapWithKeyWithAdjustWithMove (coerce . f) (coerce dm0) (coerce dm)

instance MonadTrans (DontPersistT t) where
  lift = DontPersistT

