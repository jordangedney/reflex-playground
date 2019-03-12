{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module RandomFloodFill (headElement, bodyElement) where

import Util
import JSUtil

-- Qualified -------------------------------------------------------------------

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Monoid

import qualified Reflex as R
import qualified Reflex.Dom as RD
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified Reflex.Dom.CanvasDyn as CDyn
import qualified JSDOM.CanvasRenderingContext2D as C
import qualified JSDOM.ImageData as ID

-- Direct ----------------------------------------------------------------------

import qualified GHCJS.DOM as JSDOM
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.ByteString (ByteString)
import Data.Time (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import Foreign.Ptr (Ptr)
import Data.Maybe (fromMaybe)

import JSDOM.Types (
  JSString,
  MonadJSM,
  unUint8ClampedArray,
  Uint8ClampedArray,
  JSVal,
  JSM,
  )
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import Reflex.Dom ((=:))
import Reflex.Pure (unEvent)

--------------------------------------------------------------------------------

headElement :: RD.MonadWidget t m => m ()
headElement = mkHeadElement "Random Flood Fill" "css/simple.css"


getVal :: RD.Event t a -> Maybe a
getVal e = do
  y <- unEvent e
  return y

bodyElement :: RD.MonadWidget t m => IORef Int -> m ()
bodyElement cntr = do
  (width, height) <- screenSize
  evStart <- RD.getPostBuild

  evTick <- RD.tickLossy 0.01 =<< liftIO getCurrentTime
  let temp = RD._tickInfo_n <$> evTick
  -- test <- (\y -> unEvent y temp)
  -- let h = RD.value test

  dCx <- createBlankCanvas $
          (canvasAttrs width height) <>
          ("imageRendering" =: "pixelated")

  z <- liftIO $ readIORef cntr
  evRendered <- drawWithCx' dCx ((\cx _ _ -> render z cx) <$> dCx) (() <$ evTick)
  liftIO $ modifyIORef cntr (+1)

  pure ()

render :: MonadJSM m => Int -> CanvasRenderingContext2D -> m ()
render cntr cx = do
  let (w, h) = (cntr, cntr)
  let blackPixel = [0x00, 0x00, 0x00,0xff]
  let smallImage = concat $ take (w * h) $ repeat blackPixel
  img <- makeImageData w h smallImage
  C.putImageData cx img 0 0
  pure ()
