{-# LANGUAGE TypeApplications #-}
module Main where

import Webflex.Server
import TodoMVC2
import Reflex.Spider
import Reflex.Host.Headless
import Webflex.Base
import Reflex.Persist.Base

main :: IO ()
main = runHeadlessApp (serverTToHeadless (mapServerT (runPersistTIO "/tmp/todomvcstore") todoMVC))
