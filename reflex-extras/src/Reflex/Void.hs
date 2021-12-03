{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

-- TODO: Better description of this module.

-- | Void implementation of reflex and reflex-dom classes.
module Reflex.Void where

import Reflex
import Data.Void
import Control.Monad.Fix
import Reflex.Dom.Builder.Class
import Data.Default
import Reflex.Adjustable.Class ()
import Reflex.PostBuild.Class ()
import Control.Monad.IO.Class

data Voidflex :: *

data VoidM a
  
instance Reflex Voidflex where
  data Behavior Voidflex a
  data Event Voidflex a
  data Dynamic Voidflex a
  type PullM Voidflex = VoidM
  type PushM Voidflex = VoidM

instance Functor (Dynamic Voidflex)
instance Applicative (Dynamic Voidflex)
instance Monad (Dynamic Voidflex)

instance MonadSample Voidflex VoidM where
  sample = undefined

instance MonadHold Voidflex VoidM where
  headE = undefined
  buildDynamic = undefined
  hold = undefined
  holdDyn = undefined
  holdIncremental = undefined
  now = undefined
  

instance Functor VoidM
instance Applicative VoidM
instance Monad VoidM
instance MonadFix VoidM

data VoidDomSpace :: *
data VoidEventSpec :: (EventTag -> *) -> *

instance Default (VoidEventSpec EventResult) where
  def = undefined

instance DomSpace VoidDomSpace where
   type EventSpec VoidDomSpace = VoidEventSpec
   type RawDocument VoidDomSpace = Void
   type RawDocument VoidDomSpace = Void
   type RawTextNode VoidDomSpace = Void
   type RawCommentNode VoidDomSpace = Void
   type RawElement VoidDomSpace = Void
   type RawInputElement VoidDomSpace = Void
   type RawTextAreaElement VoidDomSpace = Void
   type RawSelectElement VoidDomSpace = Void
   addEventSpecFlags = undefined

instance NotReady Voidflex VoidM where
  notReadyUntil = undefined
  notReady = undefined

instance Adjustable Voidflex VoidM where

instance PostBuild Voidflex VoidM where
  
instance DomBuilder Voidflex VoidM where
  type DomBuilderSpace VoidM = VoidDomSpace
  textNode = undefined
  commentNode = undefined
  element = undefined
  inputElement = undefined
  textAreaElement = undefined
  selectElement = undefined
  placeRawElement = undefined
  wrapRawElement = undefined

instance PerformEvent Voidflex VoidM where
  type Performable VoidM = VoidM

instance MonadIO VoidM where

instance TriggerEvent Voidflex VoidM where
