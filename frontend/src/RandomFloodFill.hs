{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module RandomFloodFill (headElement, bodyElement) where

-- Local -----------------------------------------------------------------------

import Util

-- Qualified -------------------------------------------------------------------

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Monoid

import qualified Reflex.Dom as RD
import qualified Reflex.Dom.CanvasBuilder.Types as Canvas
import qualified Reflex.Dom.CanvasDyn as CDyn

-- Direct ----------------------------------------------------------------------

import Data.Time (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))

import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import Reflex.Dom ((=:))

--------------------------------------------------------------------------------

headElement :: RD.MonadWidget t m => m ()
headElement = mkHeadElement "Random Flood Fill" "css/simple.css"

bodyElement :: RD.MonadWidget t m =>  m()
bodyElement = do
  (width, height) <- screenSize
  evStart <- RD.getPostBuild

  RD.el "h2" $ RD.text "A Simple Clock"
  now <- liftIO getCurrentTime
  evTick <- tE $ RD.tickLossy 1 now
  let evTime = (T.pack . show . RD._tickInfo_lastUTC) <$>  evTick
  RD.dynText =<< RD.holdDyn "No ticks yet" evTime

  (canvasEl, _) <- RD.elAttr' "canvas" (canvasAttrs width height) RD.blank
  d2D <- CDyn.dContext2d ( Canvas.CanvasConfig canvasEl [] )

  pure ()

canvasAttrs :: Int -> Int -> Map.Map T.Text T.Text
canvasAttrs width height =
  ("width" =: (T.pack . show $ width)) <>
  ("height" =: (T.pack . show $ height))