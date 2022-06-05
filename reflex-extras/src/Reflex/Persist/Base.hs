{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflex.Persist.Base where

import Control.Exception
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson as A
import Data.Map (Map)
import qualified Data.Map as Map
import Reflex
import Reflex.Persist.Class
import Reflex.REWST
import System.IO
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

-- TODO: Incremental update of state.

newtype PersistT t m a = PersistT (REWST (Map Int A.Value) (Map Int A.Value) Int t m a)
  deriving newtype (Functor,Applicative,Monad,MonadFix, MonadSample t, MonadHold t
                   , PostBuild t)

instance (Monad m, Reflex t) => Persist t (PersistT t m) where
  persistE' e = PersistT $ do
    n <- modify (+ 1) >> get
    tellEvent (Map.singleton n . A.toJSON <$> e)
    ( ( ( \case
            A.Error _ -> Nothing
            A.Success a -> Just a
        )
          . A.fromJSON
      )
        <=< Map.lookup n
      )
      <$> ask

runPersistT :: (Reflex t, Monad m) => PersistT t m a -> Map Int Value -> Int -> m (a, (Event t (Map Int Value), Int))
runPersistT (PersistT m) r i = runREWST m r i

runPersistTIO :: (PerformEvent t m, Reflex t, MonadIO m,
                   MonadIO (Performable m)) => FilePath -> PersistT t m a -> m a
runPersistTIO path m = do
  liftIO . putStrLn $ "Reading persisted state"
  !contents <- liftIO $ try @IOException $ readFile path
  
  let s@(storedIdCtr, storedEnv) :: (Int, Map Int A.Value) =
        either (const (0, mempty)) (fromMaybe (0, mempty) . readMaybe) $
          contents
  liftIO . putStrLn $ "Got persisted state: \n" ++ show s
  (a, (updatedEnvE, ctr)) <- runPersistT m storedEnv 0
  performEvent_ (liftIO . writeFile path . show . (ctr,)  <$> updatedEnvE)
  pure a
