module Main where
import Reflex.Dom.Core

import Language.Javascript.JSaddle.Warp (run)

import Util
import View
import View2


main :: IO ()
main = do
  autoReload
  run 3911 $ mainWidget $ myApp >> myApp2
