{-# LANGUAGE TypeApplications #-}
module Main where

import Webflex.Server
import WebCounter
import Reflex.Spider

main = runSpiderHost $ runServer webcounter
