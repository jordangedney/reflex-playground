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

-- Direct ----------------------------------------------------------------------

import Data.Time (getCurrentTime)
import Control.Monad.Trans (liftIO)
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)

--------------------------------------------------------------------------------

headElement :: RD.MonadWidget t m => m ()
headElement = mkHeadElement "Random Flood Fill" "css/simple.css"

bodyElement :: RD.MonadWidget t m =>  m()
bodyElement = do
  RD.el "h2" $ RD.text "A Simple Clock"
  now <- liftIO getCurrentTime
  evTick <- RD.tickLossy 1 now
  let evTime = (T.pack . show . RD._tickInfo_lastUTC) <$>  evTick
  RD.dynText =<< RD.holdDyn "No ticks yet" evTime