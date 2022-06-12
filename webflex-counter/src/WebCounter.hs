{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module WebCounter where

import Reflex
import Reflex.Dom
import Webflex.Class

import Control.Monad.Fix
import qualified Data.Text as T

webcounter ::
  forall c s m.
  ( WebM c s m,
    Reflex s,
    Reflex c,
    DomBuilder c (CM m),
    PostBuild c (CM m),
    MonadHold c (CM m),
    MonadHold s (SM m),
    MonadFix (SM m),
    Monad m
  ) =>
  m ()
webcounter = do
  clicks :: Event c () <- liftC $ button "click me"
  clicksAtS :: Event s (C m, ()) <- atSE clicks
  countAtS :: Dynamic s Integer <- liftS $ count clicksAtS
  countC :: Dynamic c Integer <- atAllCDyn 0 countAtS
  liftC_ $ el "div" $ dynText (fmap (T.pack . show) countC)
  pure ()
