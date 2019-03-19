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
import Data.Word (Word8)

--------------------------------------------------------------------------------

headElement :: RD.MonadWidget t m => m ()
headElement = mkHeadElement "Random Flood Fill" "css/simple.css"


-- XXX Prob not even necessary
alignToPixels x = 2 * ((x `floorDiv` pixelSize) `floorDiv` 2)

bodyElement :: RD.MonadWidget t m => IO Integer -> m ()
bodyElement randomNum = do
  (width'', height'') <- screenSize
  let (width', height') = (width'' - 20, height'' - 20)
  let (width, height) = (alignToPixels width', alignToPixels height')

  evStart <- RD.getPostBuild
  c <- liftIO $ coordsToVisit width height randomNum
  let pixelsToDraw = c <$ evStart

  evTick <- RD.tickLossy 0.01 =<< liftIO getCurrentTime
  -- evTick <- RD.tickLossy 1 =<< liftIO getCurrentTime
  dyGameTick <- RD.count evTick
  let dyState = RD.traceDyn "" $ (getState width' width height' height c) <$> dyGameTick

  dCx <- createBlankCanvas $
          ("style" =: "image-rendering: pixelated; background-color: white;") <>
          (canvasAttrs width' height')

  let renderer = (\state cx _ -> render state cx) <$> dyState
  evRendered <- CDyn.drawWithCx dCx renderer (() <$ evTick)

  pure ()

type Coords = [((Integer, Integer), Pixel)]

data State = State
  { screenWidth :: Integer
  , width :: Integer
  , screenHeight :: Integer
  , height :: Integer
  , toDraw :: [((Integer, Integer), Pixel)]
  , gameTick :: Integer
  } deriving (Show, Eq, Ord)

getState sw w sH h init gameTick = State sw w sH h coords gameTick
  where coords = zip init $ repeat blackPixel

inBounds :: (Ord b, Ord a, Num b, Num a) => (b, a) -> b -> a -> Bool
inBounds (x, y) w h = and [x >= 0, x <= w, y >= 0, y <= h]

x `floorDiv` y = (\(r, _) -> r) $ x `divMod` y

startingCoords :: Integer -> Integer -> IO Integer -> IO (Integer, Integer)
startingCoords maxWidth maxHeight rnd = do
  startingX <- rnd
  startingY <- rnd
  let x = (startingX `mod` maxWidth)
  let y = (startingY `mod` maxHeight)
  if inBounds (x + bW, y + bH) (bound maxWidth) (bound maxHeight)
  then pure (x + bW, y + bH)
  else startingCoords maxWidth maxHeight rnd
  -- else pure [(0,0)]
  where bound num = num - (num `floorDiv` 4)
        bW = maxWidth `floorDiv` 8
        bH = maxWidth `floorDiv` 8
        mW = bound maxWidth
        mH = bound maxHeight

coordsToVisit :: Integer -> Integer -> IO Integer -> IO [(Integer, Integer)]
coordsToVisit maxWidth maxHeight rnd = do
  (x, y) <- liftIO $ startingCoords maxWidth maxHeight rnd
  let rest = [(x + x', y) | x' <- [1..maxWidth]]
  pure $ [(x, y)] ++ takeWhile (\coord -> inBounds coord maxWidth maxHeight) rest

render :: MonadJSM m => State -> CanvasRenderingContext2D -> m ()
render (State sW w sH h [] t) cx = do pure ()
render state@(State _ w _ h (p:pixels) t) cx = do
  let ((x, y), pix) = p
  drawPixel cx pix (x * pixelSize) (y * pixelSize)
  render state { toDraw = pixels } cx

-- Drawing ---------------------------------------------------------------------

-- pixelSize has to be even, or you get a javascript exception, dunno why
pixelSize :: Integer
pixelSize = 2

mkPixel color = concat $ take (fromIntegral (pixelSize * pixelSize)) $ repeat color
pos x = x * pixelSize

blackPixel :: Pixel
blackPixel = mkPixel [0x00, 0x00, 0x00,0xff]

drawPixel :: MonadJSM m => CanvasRenderingContext2D -> Pixel -> Integer -> Integer ->  m ()
drawPixel cx pixel xPos yPos = do
  img <- makeImageData pixelSize pixelSize pixel
  C.putImageData cx img (fromIntegral xPos) (fromIntegral yPos)
