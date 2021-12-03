{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Reflex.Extra.Orphans where

import Reflex
import Control.Monad.Trans.Control
import Control.Monad.Trans.Compose
import Control.Monad.Morph
import Reflex.Dom hiding (run)
import Control.Monad.Fix
import Control.Monad
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet.Internal
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Semigroup
import Data.Some (Some)
import Data.Tuple
import Data.Type.Equality
import Data.Functor.Compose
import Data.Functor.Misc
import Data.GADT.Compare
import Data.Functor.Identity
import qualified Data.Patch.MapWithMove as MapWithMove
import Data.These
import Control.Monad.State.Strict
import Data.Align

-- TODO: signature copied from https://hackage.haskell.org/package/in-other-words-0.2.0.0/docs/Control-Effect-Carrier-Internal-Compose.html
instance (MonadTransControl t, MonadTransControl u, forall m. Monad m => Monad (u m)
         , Control.Monad.Morph.MFunctor t) => MonadTransControl (ComposeT t u) where
  type StT (ComposeT t u) a = StT u (StT t a)
  liftWith = defaultLiftWith2 ComposeT getComposeT
  {-# INLINEABLE liftWith #-}
  restoreT = defaultRestoreT2 ComposeT
  {-# INLINEABLE restoreT #-}

newtype ApplyT (f :: (* -> *) -> * -> *) (m :: * -> *) a = ApplyT { unApplyT :: f m a }
  deriving (Functor,Applicative,Monad,MonadFix,MonadTrans,MonadTransControl
           , EventWriter t w
           , NotReady t)

-- TODO: Had to add MonadFix here for DynamicWriter. How to deal with even more constraints?
type RunF f = forall (m :: * -> *) (a :: *).
  (Monad m, MonadFix m) => f m a -> m (StF f a)

  -- TODO: How do I write this default?
  -- default liftWithF :: forall m a.
  --   (MonadTransControl f, forall x. StT f x ~ x, StF f ~ Identity, Monad m)
  --   => (RunF f -> m a) -> f m a
  --   -- undefined :: m (g a) -> m (Compose Identity g a)
  -- liftWithF f = liftWith (\run -> f (\x -> fmap Identity $ run $ x))
class MonadBaseControlF f where
  type StF f :: * -> *
  type StF f = Identity
  liftWithF :: (Monad m) => (RunF f -> m a) -> f m a
  restoreStF :: (Monad m) => m (StF f a) -> f m a
  

class (MonadBaseControlF f) => AdjustStF t f where
  adjustE :: (MonadHold t m) => StF f a -> Event t (StF f b) -> f m (a, Event t b)
  adjustIntMap :: (MonadHold t m) => IntMap (StF f a)
     -> Event t (PatchIntMap (StF f a))
     -> f m (IntMap a, Event t (PatchIntMap a))
  -- TODO: Had to add MonadFix m for DynamicWriter, can I avoid hard coding these constraints?
  adjustDMap :: (GCompare k, MonadHold t m, MonadFix m) 
    => DMap k (Compose (StF f) v)
     -> Event t (PatchDMap k (Compose (StF f) v))
     -> f m (DMap k v, Event t (PatchDMap k v))
  adjustDMapWithMove :: (GCompare k, MonadHold t m, MonadFix m)
    => DMap k (Compose (StF f) v)
     -> Event t (PatchDMapWithMove k (Compose (StF f) v))
     -> f m (DMap k v, Event t (PatchDMapWithMove k v))

instance (Reflex t, Monoid w) => MonadBaseControlF (DynamicWriterT t w) where
  type StF (DynamicWriterT t w) = (,) (Dynamic t w)
  liftWithF f = lift $ f (fmap swap . runDynamicWriterT)
  restoreStF m = do
    (w,a) <- lift m
    tellDyn w
    pure a

newtype DynamicWriterTLoweredResult t w v a = DynamicWriterTLoweredResult (v a, Dynamic t w)

instance (Reflex t, Monoid w) => AdjustStF t (DynamicWriterT t w) where
  adjustE result0 result' = do
    tellDyn . join =<< holdDyn (fst result0) (fst <$> result')
    return (snd result0, snd <$> result')
  adjustDMap = adjustDynamicWriterDMap mapPatchDMap weakenPatchDMapWith mergeDynIncremental
  adjustDMapWithMove = adjustDynamicWriterDMap mapPatchDMapWithMove weakenPatchDMapWithMoveWith mergeDynIncrementalWithMove

mergeDynIncrementalWithMove :: forall t k v. (Reflex t, Ord k) => Incremental t (PatchMapWithMove k (Dynamic t v)) -> Incremental t (PatchMapWithMove k v)
mergeDynIncrementalWithMove a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ alignWith f addedAndRemovedValues changedValues
  where changedValues = mergeMapIncrementalWithMove $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ fmap unsafePatchMapWithMove . mapM (mapM (sample . current)) . unPatchMapWithMove
--        f :: These (PatchMapWithMove k v) (Map k v) -> PatchMapWithMove k v
        f x = unsafePatchMapWithMove $
          let (p, changed) = case x of
                This p_ -> (unPatchMapWithMove p_, mempty)
                That c -> (mempty, c)
                These p_ c -> (unPatchMapWithMove p_, c)
              (pWithNewVals, noLongerMoved) = flip runState [] $ forM p $ MapWithMove.nodeInfoMapMFrom $ \case
                MapWithMove.From_Insert v -> return $ MapWithMove.From_Insert v
                MapWithMove.From_Delete -> return MapWithMove.From_Delete
                MapWithMove.From_Move k -> case Map.lookup k changed of
                  Nothing -> return $ MapWithMove.From_Move k
                  Just v -> do
                    modify (k:)
                    return $ MapWithMove.From_Insert v
              noLongerMovedMap = Map.fromList $ fmap (, ()) noLongerMoved
          in Map.differenceWith (\e _ -> Just $ MapWithMove.nodeInfoSetTo Nothing e) pWithNewVals noLongerMovedMap --TODO: Check if any in the second map are not covered?
    
mergeDynIncremental :: (Reflex t, Ord k) => Incremental t (PatchMap k (Dynamic t v)) -> Incremental t (PatchMap k v)
mergeDynIncremental a = unsafeBuildIncremental (mapM (sample . current) =<< sample (currentIncremental a)) $ addedAndRemovedValues <> changedValues
  where changedValues = fmap (PatchMap . fmap Just) $ mergeMapIncremental $ mapIncrementalMapValues updated a
        addedAndRemovedValues = flip pushAlways (updatedIncremental a) $ \(PatchMap m) -> PatchMap <$> mapM (mapM (sample . current)) m

mapIncrementalMapValues :: (Reflex t, Patch (p v), Patch (p v'), PatchTarget (p v) ~ f v, PatchTarget (p v') ~ f v', Functor p, Functor f) => (v -> v') -> Incremental t (p v) -> Incremental t (p v')
mapIncrementalMapValues f = unsafeMapIncremental (fmap f) (fmap f)


adjustDynamicWriterDMap :: forall t w k v' p p' m. (PatchTarget (p' (Some k) (Dynamic t w)) ~ Map (Some k) (Dynamic t w), PatchTarget (p' (Some k) w) ~ Map (Some k) w, Patch (p' (Some k) w), Patch (p' (Some k) (Dynamic t w)), MonadFix m, Monoid w, Reflex t, MonadHold t m)
  => ((forall a. Compose ((,) (Dynamic t w)) v' a -> v' a) -> p k (Compose ((,) (Dynamic t w)) v') -> p k v')
  -> ((forall a. Compose ((,) (Dynamic t w)) v' a -> Dynamic t w) -> p k (Compose ((,) (Dynamic t w)) v') -> p' (Some k) (Dynamic t w))
  -> (Incremental t (p' (Some k) (Dynamic t w)) -> Incremental t (p' (Some k) w))
  -> DMap k (Compose ((,) (Dynamic t w)) v')
  -> Event t (p k (Compose ((,) (Dynamic t w)) v'))
  -> DynamicWriterT t w m (DMap k v', Event t (p k v'))
adjustDynamicWriterDMap mapPatch weakenPatchWith mergeMyDynIncremental result0 result' = do
  let getValue = snd . getCompose
      getWritten = fst . getCompose
      liftedResult0 = DMap.map getValue result0
      liftedResult' = ffor result' $ mapPatch getValue
--      liftedWritten0 :: Map (Some k) (Dynamic t w)
      liftedWritten0 = weakenDMapWith getWritten result0
      liftedWritten' = ffor result' $ weakenPatchWith getWritten
  --TODO: We should be able to improve the performance here by incrementally updating the mconcat of the merged Dynamics
  i <- holdIncremental liftedWritten0 liftedWritten'
  tellDyn $ fmap (mconcat . Map.elems) $ incrementalToDynamic $ mergeMyDynIncremental i
  return (liftedResult0, liftedResult')


instance (Reflex t, Semigroup w) => MonadBaseControlF (EventWriterT t w) where
  type StF (EventWriterT t w) = (,) (Event t w)
  liftWithF f = lift $ f (fmap swap . runEventWriterT)
  restoreStF m = do
    (w,a) <- lift m
    tellEvent w
    pure a

instance (Reflex t, Semigroup w) => MonadTransControl (EventWriterT t w) where
  type StT (EventWriterT t w) a = (a, Event t w)
  liftWith f = lift $ f runEventWriterT
  restoreT msta = do
    (a,w) <- lift msta
    tellEvent w
    pure a



adjustEventWriterDMap :: forall t m p p' w k v'
  .  ( Reflex t
     , MonadHold t m
     , Semigroup w
     , Patch (p' (Some k) (Event t w))
     , PatchTarget (p' (Some k) (Event t w)) ~ Map (Some k) (Event t w)
     , GCompare k
     , Patch (p' (Some k) w)
     , PatchTarget (p' (Some k) w) ~ Map (Some k) w
     )
  => ((forall a. Compose ((,) (Event t w)) v' a -> v' a) -> p k (Compose ((,) (Event t w)) v') -> p k v')
  -> ((forall a. Compose ((,) (Event t w)) v' a -> Event t w) -> p k (Compose ((,) (Event t w)) v') -> p' (Some k) (Event t w))
  -> (Incremental t (p' (Some k) (Event t w)) -> Event t (PatchTarget (p' (Some k) w)))
  -> (Event t (p' (Some k) (Event t w)) -> Event t (p' (Some k) w))
  -> (DMap k (Compose ((,) (Event t w)) v'))
  -> (Event t (p k (Compose ((,) (Event t w)) v')))
  -> EventWriterT t w m (DMap k v', Event t (p k v'))
adjustEventWriterDMap mapPatch weakenPatchWith mergePatchIncremental coincidencePatch children0 children' = do
    let result0 = DMap.map (snd . getCompose) children0
        result' = fmapCheap (mapPatch $ snd . getCompose)  children'
        requests0 = weakenDMapWith (fst . getCompose) children0
        requests' = fmapCheap (weakenPatchWith $ fst . getCompose) children'
    e <- switchHoldPromptOnlyIncremental mergePatchIncremental coincidencePatch requests0 requests'
    tellEvent $ fforMaybeCheap e $ \m ->
      case Map.elems m of
        [] -> Nothing
        h : t -> Just $ sconcat $ h :| t
    return (result0, result')

instance ( Adjustable t m, Monad m, Monad (f m)
         , AdjustStF t f
         , MonadHold t m
         , MonadFix m
         ) => Adjustable t (ApplyT f m) where
  runWithReplace m e = ApplyT $ (uncurry adjustE =<<) $ liftWithF $ \run -> do
    runWithReplace (run . unApplyT $ m) (fmapCheap (run . unApplyT) e)
  traverseIntMapWithKeyWithAdjust f im e = ApplyT $ (uncurry adjustIntMap =<<) $ liftWithF $ \run ->
      traverseIntMapWithKeyWithAdjust (\k -> run . unApplyT . f k) im e
  traverseDMapWithKeyWithAdjust f dm e =
    ApplyT $ (uncurry adjustDMap =<<) $ liftWithF $ \run ->
       traverseDMapWithKeyWithAdjust
          (\k -> fmap Compose . run . unApplyT . f k)
          dm
          e
  traverseDMapWithKeyWithAdjustWithMove f dm e =
    ApplyT $ (uncurry adjustDMapWithMove =<<) $ liftWithF $ \run ->
       traverseDMapWithKeyWithAdjustWithMove
          (\k -> fmap Compose . run . unApplyT . f k)
          dm
          e

-- TODO: get rid of m
instance ( MonadTrans f, m ~ f m', DomBuilder t m', Monad m
         , NotReady t m
         , MonadTransControl f
         , Adjustable t m
         , AdjustStF t f
         , MonadHold t m'
         , MonadFix m'
         ) => DomBuilder t (ApplyT f m') where
  type DomBuilderSpace (ApplyT f m') = DomBuilderSpace m'
  element tg cfg m = ApplyT $ do
    (e,st) <- liftWith (\run -> element tg cfg (run $ unApplyT m))
    (e,) <$> restoreT (pure st)
  selectElement cfg m = ApplyT $ do
    (e,st) <- liftWith (\run -> selectElement cfg (run $ unApplyT m))
    (e,) <$> restoreT (pure st)
