{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecursiveDo #-}

module View where

import qualified Data.Map        as Map
import           Data.Monoid
import qualified Data.Text       as T
import           Reflex.Dom.Core

myApp :: (forall x. Widget x ())
myApp = do
  simpleElements
  exDynAttr
  exFoldDyn
  exFoldDynWith2Button

simpleElements :: (forall x. Widget x ())
simpleElements = do
  el "div" $ do
    elClass "h3" "mainTitle" $
      text "This is an example for some simple elements"
    el "p" $ text "This is a paragraph"
    el "span" $ text "this is a span"
    el "ul" $ do
      el "li" $ text "1"
      el "li" $ text "2"
      el "li" $ text "3"
  el "div" $ do
    elClass "h3" "mainTitle" $ text "This is an example with buttons"
    display =<< count =<< button "Another button"
    el "br" $ return ()
    display =<< count =<< button "Another button"
  el "div" $ do
    elClass "h3" "mainTitle" $ text "This is an example with link"
    elAttr "a" attrs $ text "Click to Google"
  where
    attrs :: Map.Map T.Text T.Text
    attrs = ("target" =: "_blank") <> ("href" =: "http://www.google.com")

exDynAttr :: (forall x. Widget x ())
exDynAttr = do
  el "div" $ do
    elClass "h3" "mainTitle" $ text "This is an example with dynamic attrs"
    rec
      dynBool <- toggle False evClick
      let dynAttrs = attrsDyn <$> dynBool
      elDynAttr "h1" dynAttrs $ text "Changing color"
      evClick <- button "Change Color"
    return ()
  where attrsDyn :: Bool -> Map.Map T.Text T.Text
        attrsDyn b = "style" =: ("color: " <> color b)
          where
            color True = "red"
            color _ = "green"

exFoldDyn :: (forall x. Widget x ())
exFoldDyn = do
  rec
    el "h3" $ text "Counter as a fold"
    numbs <- foldDyn (+) (0 :: Int)  (2 <$ evIncr)
    el "div" $ display numbs
    evIncr <- button "Increment"
  return ()

exFoldDynWith2Button :: MonadWidget t m => m ()
exFoldDynWith2Button = do
  rec
    el "h3" $ text "Combining 2 buttons with leftmost or mergeWith"
    counts <- foldDyn (+) (0 :: Int) $ leftmost [1 <$ evIncr, -1 <$ evDecr]
    countsMerge <- foldDyn (+) (0 :: Int) $ mergeWith (+) [1 <$ evIncr, -1 <$ evDecr]
    el "span" $ display counts
    evIncr <- button "Increment"
    evDecr <- button "Decrement"
    el "span" $ display countsMerge
  return ()
