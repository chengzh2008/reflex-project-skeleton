{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecursiveDo #-}

module View2 where

import Control.Monad.Trans (liftIO)
import           Reflex.Dom.Core
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid
import Data.Maybe (fromJust, fromMaybe)
import Data.Time

myApp2 :: (forall x. Widget x ())
myApp2 = do
  exDropdown
  sp
  exRangeInput
  sp
  exRangeInput2
  sp
  exRangeInput3
  sp
  exButtonEnableDisable
  sp
  exRadioGroup
  sp
  exTimer
  sp
  exXhrRequest


sp :: MonadWidget t m => m ()
sp =
  el "h3" $ text "*********************************************************"

exDropdown :: MonadWidget t m => m ()
exDropdown = do
  el "div" $ do
    el "h3" $ text "Dropdown"
    text "Select country "
    dd <- dropdown 2 (constDyn countries) def
    el "p" $ return ()
    let selItem = result <$> value dd
    dynText selItem

countries :: Map.Map Int T.Text
countries = Map.fromList [(1, "France"), (2, "Switzerland"), (3, "Germany"), (4, "Italy"), (5, "USA")]

result :: Int -> T.Text
result key = "You selected: " <> fromJust (Map.lookup key countries)

exRangeInput :: MonadWidget t m => m ()
exRangeInput = do
  el "h3" $ text "Range Input"
  rg <- rangeInput def
  el "p" blank
  display $ _rangeInput_value rg
  return ()

exRangeInput2 :: MonadWidget t m => m ()
exRangeInput2 = do
  el "h3" $ text "Range Input2"
  rg <- rangeInput $ def & attributes .~ constDyn ("min" =: "-100")
  el "p" blank
  display $ _rangeInput_value rg
  return ()

exRangeInput3 :: MonadWidget t m => m ()
exRangeInput3 = do
  el "h3" $ text "Range Input"
  rg <-
    rangeInput $
    def & attributes .~
    constDyn
      ("min" =: "-100" <> "max" =: "100" <> "value" =: "0" <> "step" =: "10" <>
       "list" =:
       "powers")
  elAttr "datalist" ("id" =: "powers") $ do
    elAttr "option" ("value" =: "0") blank
    elAttr "option" ("value" =: "-30") blank
    elAttr "option" ("value" =: "50") blank
  el "p" blank
  display $ _rangeInput_value rg
  return ()

exButtonEnableDisable :: MonadWidget t m => m ()
exButtonEnableDisable = do
  el "div" $ do
    el "h3" $ text "Button enabled / disabled"
    cb <- el "label" $ do
      cb1 <- checkbox True def
      text "Enable or Disable the button"
      return cb1
    el "p" blank
    counter :: Dynamic t Int <- count =<< disaButton (_checkbox_value cb) "Click me"
    el "p" blank
    display counter

-- | A button that can be enabled and disabled
disaButton :: MonadWidget t m
            => Dynamic t Bool -- ^ enable or disable button
            -> T.Text         -- ^ Label
            -> m (Event t ())
disaButton enabled label = do
    let attrs = ffor enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

-- | A little helper function for data types in the *Monoid* type class:
-- If the boolean is True, return the first parameter, else return the null or empty element of the monoid
monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty


exRadioGroup :: MonadWidget t m => m ()
exRadioGroup = do
  el "div" $ do
    rec
      el "h2" $ text "Own Radio buttons"
      let group = "g"
      let dynAttrs = styleMap <$> dynColor
      evRad1 <- radioBtn "orange" group Orange dynAttrs
      evRad2 <- radioBtn "green" group Green dynAttrs
      evRad3 <- radioBtn "red" group Red dynAttrs
      let evRadio = (T.pack . show) <$> leftmost [evRad1, evRad2, evRad3]
      dynColor <- holdDyn "lightgrey" evRadio
    return ()

data Color = White | Red | Orange | Green
  deriving (Eq, Ord, Show)

-- | Helper function to create a radio button
radioBtn :: (Eq a, Show a, MonadWidget t m) => T.Text -> T.Text -> a -> Dynamic t (Map.Map T.Text T.Text)-> m (Event t a)
radioBtn label group rid dynAttrs = do
  ev <-
    elDynAttr "label" dynAttrs $ do
      (rb1, _) <-
        elAttr'
          "input"
          ("name" =: group <> "type" =: "radio" <> "value" =: T.pack (show rid))
          blank
      text label
      return $ domEvent Click rb1
  return $ rid <$ ev

styleMap :: T.Text -> Map.Map T.Text T.Text
styleMap c = "style" =: ("background-color: " <> c)


exTimer :: MonadWidget t m => m ()
exTimer = do
  el "h3" $ text "A Simple Clock"
  now <- liftIO getCurrentTime
  evTick <- tickLossy 2 now
  let evTime = (T.pack . show . _tickInfo_lastUTC) <$>  evTick
  dynText =<< holdDyn "No ticks yet" evTime


exXhrRequest :: MonadWidget t m => m ()
exXhrRequest = do
  el "div" $ do
    el "h3" $ text "Swiss Meteo Data (raw version)"
    text "Choose station: "
    dd <- dropdown "BER" (constDyn stations) def
    -- Build and send the request
    evStart <- getPostBuild
    let evCode = tagPromptlyDyn (value dd) $ leftmost [ () <$ _dropdown_change dd, evStart]
    evRsp <- performRequestAsync $ buildReq <$> evCode
    -- Display the whole response
    el "h5" $ text "Response Text:"
    let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
    dynText =<< holdDyn "" evResult
    return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" ("https://opendata.netcetera.com/smn/smn/" <> code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch")]
