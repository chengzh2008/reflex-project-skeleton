{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where

import Reflex.Dom.Core
import Language.Javascript.JSaddle.Warp (run)

import           System.Process (callProcess)
import           Control.Concurrent (threadDelay, forkIO)

main :: IO ()
main = do
  autoReload
  run 3911 $ mainWidget myApp


myApp :: (forall x. Widget x ())
myApp = do
  display =<< count =<< button "Click Me"
  display =<< count =<< button "Click Me3"



autoReload :: IO ()
autoReload = do
  _ <-
    forkIO $ do
      threadDelay 500000 -- small delay so startGUI can start listening
      callProcess "refresh" ["Chrome"]
  return ()

