{-# LANGUAGE TypeApplications #-}
module Main where

import Webflex.Server
import TodoMVC2
import Reflex.Spider
import Reflex.Host.Headless

main :: IO ()
main = runServer todoMVC
