{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
-- This is maybe a bit like continuations?

module Reflex.Wormhole.Base where

import Reflex.Wormhole.Class
import Reflex
import Control.Monad.Reader
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import GHC.Exts (Any)
import Reflex.Id.Class
import Unsafe.Coerce
import Data.Semigroup hiding (Any(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Functor.Misc (Const2(..))
import Reflex.Dom (DomBuilder(..))
import Control.Monad.Trans.Control
import Reflex.Extra.Orphans ()
import Control.Monad.Trans.Compose
import Reflex.Persist.Class

type WormholeComp i t = (ComposeT (EventWriterT t (MonoidalMap i [Any])) (ReaderT (i -> Event t [Any])))

newtype WormholeT i t m a = WormholeT
  { unWormholeT :: EventWriterT t (MonoidalMap i [Any]) (ReaderT (i -> Event t [Any]) m) a }
  deriving newtype (Functor,Applicative,Monad,MonadFix,MonadSample t,MonadHold t, NotReady t, PostBuild t,MonadIO, GetId
                   , Persist t)
  deriving (MonadTrans,MonadTransControl) via WormholeComp i t
--  deriving (DomBuilder t) via ApplyT (WormholeComp i t) m

type WormholeT' t m = WormholeT (Id m) t m

unsafeSconcat :: Semigroup a => [a] -> a
unsafeSconcat = sconcat . NonEmpty.fromList

runWormholeT :: (Reflex t, MonadFix m, Ord (Id m)) => WormholeT (Id m) t m a -> m a
runWormholeT (WormholeT m) = mdo
  let (EventSelector selector) = fanMap (fmap MonoidalMap.getMonoidalMap evts)
  ~ (a,evts) <- runReaderT (runEventWriterT m) (selector . Const2)
  pure a

instance (Reflex t, Monad m, i ~ (Id m), GetId m, Ord (Id m)) => Wormholed t (WormholeT i t m) where
  wormhole = WormholeT $ do
    i <- lift . lift $ getId
    e <- asks (fmap (unsafeSconcat . fmap unsafeCoerce) . ($ i))
    pure (e, WormholeT . tellEvent . fmap (MonoidalMap.singleton i . (:[]) . unsafeCoerce))

instance (Adjustable t m, MonadHold t m, MonadFix m, Ord i) => Adjustable t (WormholeT i t m) where
  runWithReplace m e = WormholeT $ runWithReplace (unWormholeT m) (unWormholeT <$> e)
  traverseIntMapWithKeyWithAdjust f im e =
    WormholeT $ traverseIntMapWithKeyWithAdjust (\k -> unWormholeT . f k)
             im
             e
  traverseDMapWithKeyWithAdjust f dm e =
    WormholeT $ traverseDMapWithKeyWithAdjust
          (\k -> unWormholeT . f k)
          dm
          e
  traverseDMapWithKeyWithAdjustWithMove f dm e =
    WormholeT $ traverseDMapWithKeyWithAdjustWithMove
          (\k -> unWormholeT . f k)
          dm
          e
instance ( DomBuilder t m
         , MonadHold t m
         , NotReady t m
         , Adjustable t m
         , MonadFix m
         , Ord i
         ) => DomBuilder t (WormholeT i t m) where
  type DomBuilderSpace (WormholeT i t m) = DomBuilderSpace m
  element tg cfg m = WormholeT $ do
    ~(e,st) <- liftWith (\run -> element tg cfg (run $ unWormholeT m))
    (e,) <$> restoreT (pure st)
  selectElement cfg m = WormholeT $ do
    ~(e,st) <- liftWith (\run -> selectElement cfg (run $ unWormholeT m))
    (e,) <$> restoreT (pure st)

instance (PerformEvent t m) => PerformEvent t (WormholeT i t m) where
  type Performable (WormholeT i t m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance (TriggerEvent t m) => TriggerEvent t (WormholeT i t m) where
  newTriggerEvent = lift newTriggerEvent
  newTriggerEventWithOnComplete = lift newTriggerEventWithOnComplete
  newEventWithLazyTriggerWithOnComplete = lift . newEventWithLazyTriggerWithOnComplete
