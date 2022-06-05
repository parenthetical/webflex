{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Webflex.Base where

import Control.Monad.Writer

import Reflex
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Map.Monoidal (MonoidalMap(..))
import Webflex.Class
import Reflex.Void
import Control.Monad.Trans ()
import Control.Monad.Reader
import Control.Monad.State.Lazy
import qualified Data.Aeson as A
import Data.Void
import Data.Aeson as A
import Reflex.REWST

newtype ClientT t m a =
  ClientT { client :: REWST (Event t (Map Int Value), Dynamic t Bool) (Map Int Value) Int t m a }
  deriving (Functor,Applicative,Monad,MonadFix,MonadTrans)

instance (Reflex t, Monad m) => WebM t Voidflex (ClientT t m) where
  type C (ClientT t m) = Void
  type CM (ClientT t m) = m
  type SM (ClientT t m) = VoidM
  askConnected = ClientT $ asks snd
  askConnections = pure (error "Server-side value evaluated in client-side context (connected).")
  liftC' = fmap pure . lift
  liftS' _ = pure (pure (error "Server-side value evaluated in client-side context (liftServer). How did you get here?"))
  atCE _ = ClientT $ do
    n <- modify (+ 1) >> get
    let convert = (\case A.Error _ -> error "TODO handle unexpected data"
                         A.Success r -> Just r)
                  . A.fromJSON
    asks (mapMaybe (convert <=< Map.lookup n) . fst)
  atSE e = ClientT $ do
    n <- modify (+ 1) >> get
    tellEvent (fmap (Map.singleton n . A.toJSON) e)
    pure (error "Server-side value evaluated in client-side context (atServerE_).")

newtype ServerT i t m a =
  ServerT { server :: REWST
                      (Event t (i, (Map Int Value)), Incremental t (PatchMap i ()))
                      (MonoidalMap i (Map Int Value))
                      Int
                      t
                      m
                      a
          }
  deriving (Functor,Applicative,Monad,MonadFix,MonadTrans)

mapServerT :: (forall x. m x -> n x) -> ServerT i t m a -> ServerT i t n a
mapServerT f (ServerT x) = ServerT (mapREWST f x)

instance ( Ord i, Reflex t, Monad m
         ) => WebM Voidflex t (ServerT i t m) where
  type C (ServerT i t m) = i
  type CM (ServerT i t m) = VoidM
  type SM (ServerT i t m) = m
  askConnections = ServerT (asks snd)
  askConnected = pure (error "Client-side value evaluated in server-side context. How did you get here?")
  liftC' _ = pure (pure (error "Client-side value evaluated in server-side context. How did you get here?"))
  liftS' = fmap pure . lift
  atCE e = ServerT $ do
    n <- modify (+ 1) >> get
    tellEvent (fmap (MonoidalMap . fmap (Map.singleton n . toJSON)) e)
    pure (error "Client-side value evaluated in server-side context.")
  atSE _ = ServerT $ do
    n <- modify (+ 1) >> get
    let convert = (\case Error _ -> error "TODO handle unexpected data"
                         Success r -> Just r)
                  . fromJSON
    asks (mapMaybe (\(i, vals) ->
                           fmap (i,) (convert <=< Map.lookup n $ vals))
              . fst)
