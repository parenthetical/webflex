{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import WebCounter
import Webflex.Client
import Reflex
import Reflex.Wormhole.Base
import Reflex.Wormhole.Class
import Control.Monad.IO.Class
import Control.Monad.Fix
import Reflex.Id.Impure ( runIdT', IdT )
import Data.IORef
import Spaceflex.Web.Class
import Spaceflex.Web.Base
import Reflex.Dom

main = mainWidget $ clientWidget $ webcounter
