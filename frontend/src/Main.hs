{-# LANGUAGE OverloadedStrings #-}

module Main where

import Reflex.Dom.Core
import Language.Javascript.JSaddle.Warp (run)

main :: IO ()
main = run 3911 $ mainWidget $ text "hi Zhihong "
