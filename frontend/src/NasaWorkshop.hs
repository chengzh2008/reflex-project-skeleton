{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE RecursiveDo #-}

module NasaWorkshop where

import           Reflex.Dom.Core
import GHC.Generics
import Data.Text
import Data.Maybe
import Data.Monoid
import Data.Aeson
import qualified Data.Map as M



myAppNasaWS :: forall t m. MonadWidget t m => m ()
myAppNasaWS = do
  sp


sp :: MonadWidget t m => m ()
sp = do
  text "NASA workshop"
  t <- textInput def
  let apiKey = _textInput_value t
  b :: Event t () <- button "Send Request"
  let apiKeyButtonEvent :: Event t Text = tagPromptlyDyn apiKey b
      apiKeyEnterEvent :: Event t Text = tagPromptlyDyn apiKey $ keypress Enter t
      apiKeyEvent :: Event t Text = leftmost [ apiKeyButtonEvent
                                             , apiKeyEnterEvent
                                             ]
  submittedApiKey :: Dynamic t Text <- holdDyn "No text submitted!" apiKeyEvent
  dynText submittedApiKey

  evRsp <- performRequestAsync $ buildReq <$> apiKeyEvent
  el "h5" $ text "Response :"
  let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
  dynText =<< holdDyn "Not yet!" evResult

  let decoded :: Event t (Maybe NasaPic) = fmap decodeXhrResponse evRsp
  dynPic :: Dynamic t (Maybe NasaPic) <- holdDyn Nothing decoded
  --dynPicString <- mapDyn show dynPic
  imgAttrs :: Dynamic t (M.Map Text Text) <- return . ffor dynPic $
    \np -> case np of
      Nothing -> M.empty
      Just pic -> M.singleton "src" $ url pic

  explanationText :: Dynamic t Text <- return . ffor dynPic $
    \np -> case np of
      Nothing -> "No explanation"
      Just pic -> explanation pic
  elDynAttr "img" imgAttrs $ return ()
  dynText explanationText
  return ()

buildReq :: Text -> XhrRequest ()
buildReq code = XhrRequest "GET" ("https://api.nasa.gov/planetary/apod?api_key=" <> code) def

data NasaPic = NasaPic { copyright :: Text
                       , date :: Text
                       , explanation :: Text
                       , hdurl :: Text
                       , media_type :: Text
                       , service_version :: Text
                       , title :: Text
                       , url :: Text }
             deriving (Show, Generic)
instance FromJSON NasaPic
