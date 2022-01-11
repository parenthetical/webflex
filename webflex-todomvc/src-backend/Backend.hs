{-# LANGUAGE TypeApplications #-}
module Main where

import Webflex.Server
import TodoMVC
import Reflex.Spider

main = runSpiderHost $ runServer todomvc
