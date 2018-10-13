{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecursiveDo #-}

module NasaWorkshop where

import           Reflex.Dom.Core



myAppNasaWS :: MonadWidget t m => m ()
myAppNasaWS = do
  sp


sp :: MonadWidget t m => m ()
sp = do
  el "h1" $ text "Large *** title"
  el "span" $ text "This is a span"
  el "label" $ text "Thisxxsssx is a label"
  el "h3" $ text "NASA workshop"
  el "button" $ text "click me again1"
