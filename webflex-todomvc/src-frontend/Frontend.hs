{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TodoMVC2
import Data.FileEmbed
import Reflex
import Reflex.Dom
import Webflex.Client

-- import GHCJS.DOM.Types (JSM)
main :: IO ()
main = mainWidgetWithCss $(embedFile "style.css") $ clientWidget todoMVC
