{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecursiveDo #-}

module ViewQfpl1 where

import           Reflex.Dom.Core



myAppQfpl1 :: MonadWidget t m => m ()
myAppQfpl1 = do
  sp


sp :: MonadWidget t m => m ()
sp =
  el "h3" $ text "**QFPL*******************************************************"
