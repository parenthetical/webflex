{-# LANGUAGE TypeApplications #-}
module Main where

import Webflex.Server
import WebCounter
import Reflex.Spider
import Reflex.Host.Headless

main = runHeadlessApp $ serverTToHeadless webcounter
