module Util where

import           System.Process (callProcess)
import           Control.Concurrent (threadDelay, forkIO)

autoReload :: IO ()
autoReload = do
  _ <-
    forkIO $ do
      threadDelay 500000 -- small delay so startGUI can start listening
      callProcess "refresh" ["Chrome"]
  return ()
