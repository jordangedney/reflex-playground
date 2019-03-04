{-# LANGUAGE OverloadedStrings #-}

module Canvas where

-- Local -----------------------------------------------------------------------
import Util

-- Qualified -------------------------------------------------------------------
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Monoid

import qualified Reflex.Dom as RD
import qualified JSDOM.CanvasRenderingContext2D as C

-- Direct ----------------------------------------------------------------------
import Data.Time (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))

import Reflex.Dom as RD ((=:))
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)

--------------------------------------------------------------------------------

import qualified Reflex.Dom as RD
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified Reflex.Dom.CanvasDyn as CD
import JSDOM.Types as JSDOMTypes
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)

canvasAttrs :: Int -> Int -> Map.Map T.Text T.Text
canvasAttrs width height =
  ("width" =: (T.pack . show $ width)) <>
  ("height" =: (T.pack . show $ height))

