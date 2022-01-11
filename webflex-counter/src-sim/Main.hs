{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Spaceflex.Web.Sim
import Reflex.Dom
import Reflex
import Reflex.Wormhole.Base
import Reflex.Wormhole.Class
import Control.Monad.IO.Class
import Control.Monad.Fix
import Reflex.Id.Impure ( runIdT', IdT )
import Data.IORef
import Spaceflex.Web.Class
import qualified Data.Text as T
import Spaceflex.Web.Base
import WebCounter

runImpureWormholeT :: (MonadIO m, Reflex t, MonadFix m) =>
                            WormholeT Int t (IdT m) b -> m b
runImpureWormholeT m = do
  idRef <- liftIO $ newIORef 0
  runIdT' (runWormholeT m) idRef

prog :: forall t m. (MonadIO (Performable m), DomBuilder t m,
                       PostBuild t m, PerformEvent t m, TriggerEvent t m, MonadHold t m,
                       MonadIO m, MonadFix m) => m ()
prog = do
  runImpureWormholeT $ sim webcounter webcounter
  pure ()

main :: IO ()
main = mainWidget $ prog
