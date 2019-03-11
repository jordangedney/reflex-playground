{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module RandomFloodFill (headElement, bodyElement) where

-- Local -----------------------------------------------------------------------

import Util

-- Qualified -------------------------------------------------------------------

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Monoid

import qualified Reflex.Dom as RD
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified Reflex.Dom.CanvasDyn as CDyn
import qualified JSDOM.CanvasRenderingContext2D as C

-- Direct ----------------------------------------------------------------------

import Data.Time (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))

import JSDOM.Types (JSString)
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import Reflex.Dom ((=:))

--------------------------------------------------------------------------------

headElement :: RD.MonadWidget t m => m ()
headElement = mkHeadElement "Random Flood Fill" "css/simple.css"

bodyElement :: RD.MonadWidget t m =>  m()
bodyElement = do
  (width, height) <- screenSize
  evStart <- RD.getPostBuild
  now <- liftIO getCurrentTime

  -- evTick <- tE $ RD.tickLossy 1 now
  evTick <- RD.tickLossy 1 now

  dCx <- createBlankCanvas width height
  evRendered <- CDyn.drawWithCx dCx ((\cx _ _ -> render cx) <$> dCx) (() <$ evTick)

  pure ()

render cx = do
  C.setFillStyle cx ("blue" :: JSString)
  C.fillRect cx 50  50 10 10
  C.setStrokeStyle cx ("black" :: JSString)
  C.strokeRect cx 50 50 10 10
  where
