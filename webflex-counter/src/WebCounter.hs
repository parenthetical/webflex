{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module WebCounter where

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

webcounter :: forall c s m. (DomBuilder c (CM m), WebM c s m,
                       PostBuild c (CM m), Reflex s, MonadHold c (CM m),
                       MonadHold s (SM m), MonadFix (SM m), Monad m) => m ()
webcounter = do
  clickz <- liftC $ button "click me"
  clickzAtS <- atSE clickz
  countAtS <- liftS $ count clickzAtS
  countC <- atAllCDyn (0 :: Integer) countAtS
  liftC_ $ dynText (fmap (T.pack . show) countC)
  pure ()
