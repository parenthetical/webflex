{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Webflex.Sim
import Reflex.Dom
import Reflex
import Reflex.Wormhole.Base
import Reflex.Wormhole.Class
import Control.Monad.IO.Class
import Control.Monad.Fix
import Reflex.Id.Impure ( runIdT', IdT )
import Reflex.Id.Base ( runIdT', IdT )
import Data.IORef
import Webflex.Class
import qualified Data.Text as T
import Webflex.Base
import TodoMVC2
import Reflex.Id.Base (Idnt)
import Data.FileEmbed
import Reflex.Persist.Class
import Reflex.Persist.Dont

runImpureWormholeT :: (MonadIO m, Reflex t, MonadFix m) =>
                            WormholeT Int t (Reflex.Id.Impure.IdT m) b -> m b
runImpureWormholeT m = do
  idRef <- liftIO $ newIORef 0
  Reflex.Id.Impure.runIdT' (runWormholeT m) idRef

runPureWormholeT :: (Reflex t, MonadFix m) => WormholeT Idnt t (Reflex.Id.Base.IdT m) b -> m b
runPureWormholeT m = do
  Reflex.Id.Base.runIdT' (runWormholeT m)

prog :: forall t m. (MonadIO (Performable m), TriggerEvent t m,
                   PerformEvent t m, PostBuild t m, DomBuilder t m, MonadHold t m,
                   MonadFix m) => m ()
prog = do
  runDontPersistT $ runPureWormholeT (sim todoMVC todoMVC)
  pure ()

deriving instance (DomBuilder t m) => DomBuilder t (DontPersistT t m)

main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ prog
