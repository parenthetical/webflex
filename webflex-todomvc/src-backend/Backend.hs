{-# LANGUAGE TypeApplications #-}
module Main where

import Webflex.Server
import TodoMVC2
import Reflex.Spider

main :: IO ()
main = runSpiderHost $ runServer todoMVC
