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
  sp
  exDynAttr
  sp
  exFoldDyn
  sp
  exFoldDynWith2Button
  sp
  exFoldDayWithApplication
  sp
  exTextInput
  sp
  exTextInput2
  sp
  exTextInputWhenEvent
  sp
  exTextInputWhenPress
  sp
  exTextInputSetValue
  sp
  exTextInputResetButton
  sp
  exRGBColorViewer
  sp
  exCheckbox

sp :: MonadWidget t m => m ()
sp =
  el "h4" $ text "--------------------------------------------------------"

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

exFoldDayWithApplication :: MonadWidget t m => m ()
exFoldDayWithApplication = do
  el "h3" $ text "Using foldDyn with function application"
  rec
    dynNum <- foldDyn ($) (0 :: Int) $ leftmost [(+ 1) <$ evIncr, (+ (-1)) <$ evDecr, const 0 <$ evReset]
    el "div" $ display dynNum
    evIncr <- button "Increment"
    evDecr <- button "Decrement"
    evReset <- button "Reset"
  return ()

exTextInput :: MonadWidget t m => m ()
exTextInput = do
  el "h3" $ text "Text input example"
  el "div" $ do
    el "h2" $ text "Simple Text Input"
    ti <- textInput $ def & textInputConfig_initialValue .~ "initial"
    dynText $ value ti

exTextInput2 :: MonadWidget t m => m ()
exTextInput2 = do
  el "h3" $ text "Text Input - Configuration"

  el "h4" $ text "Max Length 14"
  t1 <- textInput $ def & attributes .~ constDyn ("maxlength" =: "14")
  dynText $ _textInput_value t1

  el "h4" $ text "Initial Value"
  t2 <- textInput $ def & textInputConfig_initialValue .~ "input"
  dynText $ _textInput_value t2

  el "h4" $ text "Input Hint"
  t3 <- textInput $
        def & attributes .~ constDyn("placeholder" =: "type something")
  dynText $ _textInput_value t3

  el "h4" $ text "Password"
  t4 <- textInput $ def & textInputConfig_inputType .~ "password"
  dynText $ _textInput_value t4

  el "h4" $ text "Multiple Attributes: Hint + Max Length"
  t5 <- textInput $  def & attributes .~ constDyn ("placeholder" =: "Max 6 chars" <> "maxlength" =: "6")
  dynText $ _textInput_value t5

  el "h4" $ text "Numeric Field with initial value"
  t6 <- textInput $ def & textInputConfig_inputType .~ "number"
                        & textInputConfig_initialValue .~ "0"
  dynText $ _textInput_value t6

  el "h4" $ text "Check box"
  t7 <- textInput $ def & textInputConfig_inputType .~ "checkbox"
  dynText $ _textInput_value t7

exTextInputWhenEvent :: MonadWidget t m => m ()
exTextInputWhenEvent = do
  el "h3" $ text "Text Input - Read Value on Button Click"
  ti <- textInput def
  evClick <- button "Click Me"
  el "br" blank
  text "Contents of TextInput on last click: "
  let evText = tagPromptlyDyn (value ti) evClick
  dynText =<< holdDyn "" evText

exTextInputWhenPress :: MonadWidget t m => m ()
exTextInputWhenPress = do
  el "h3" $ text "Text Input - Read Value on 'Enter'"
  ti <- textInput def
  el "br" blank
  text "Contents of TextInput after 'Enter': "
  let evEnter = keypress Enter ti
  let evText = tagPromptlyDyn (value ti) evEnter
  dynText =<< holdDyn "" evText

exTextInputSetValue :: MonadWidget t m => m ()
exTextInputSetValue = do
  el "h3" $ text "Write into TextInput Widget"
  t1 <- textInput def
  evCopy <- button ">>>"
  let evText = tagPromptlyDyn (value t1) evCopy
  _ <- textInput $ def & setValue .~ evText
  return ()

exTextInputResetButton :: MonadWidget t m => m ()
exTextInputResetButton = do
  rec
    el "h1" $ text "Clear TextInput Widget"
    _ <- textInput $ def & setValue .~ ("" <$ evReset)
    evReset <- button "Reset"
  return ()

exRGBColorViewer :: MonadWidget t m => m ()
exRGBColorViewer = do
  el "h2" $ text "RGB Viewer"
  el "div" $ text "Enter RGB component values as numbers between 0 and 255"
  dfsRed <- labledBox "Red: "
  dfsGreen <- labledBox "Green: "
  dfsBlue <- labledBox "Blue: "
  _ <-
    textArea $
    def & attributes .~
    (styleMap <$> value dfsRed <*> value dfsGreen <*> value dfsBlue)
  return ()

labledBox :: MonadWidget t m => T.Text -> m (TextInput t)
labledBox lbl =
  el "div" $ do
    text lbl
    textInput $
      def & textInputConfig_inputType .~ "number" & textInputConfig_initialValue .~
      "0"

styleMap :: T.Text -> T.Text -> T.Text -> Map.Map T.Text T.Text
styleMap r g b =
  "style" =: mconcat ["background-color: rgb(", r, ", ", g, ", ", b, ")"]


exCheckbox :: MonadWidget t m => m ()
exCheckbox = do
  el "div" $ do
    el "h3" $ text "Checkbox (Out of the box)"
    cb <- el "label" $ do
      text "Click me"
      cb1 <- checkbox True def
      return cb1
    el "p" blank
    let dynState = checkedState <$> value cb
    dynText dynState

checkedState :: Bool -> T.Text
checkedState True = "Checkbox is checked"
checkedState _    = "Checkbox is not checked"
