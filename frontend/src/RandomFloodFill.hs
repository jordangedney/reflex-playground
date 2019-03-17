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

bodyElement :: RD.MonadWidget t m => IO Word8 -> m ()
bodyElement randomNum = do
  (width'', height'') <- screenSize
  let (width', height') = (width'' - 20, height'' - 20)
  let (width, height) = (width' `floorDiv` pixelSize, height' `floorDiv` pixelSize)

  evStart <- RD.getPostBuild

  evTick <- RD.tickLossy 0.01 =<< liftIO getCurrentTime
  -- evTick <- RD.tickLossy 1 =<< liftIO getCurrentTime
  dyGameTick <- RD.count evTick
  c <- liftIO $ startingCoords width height randomNum
  let dyState = RD.traceDyn "" $ (getSize width' height' randomNum c) <$> dyGameTick

  dCx <- createBlankCanvas $
          ("style" =: "image-rendering: pixelated") <>
          (canvasAttrs width' height')

  let renderer = (\state cx _ -> render state cx) <$> dyState
  evRendered <- CDyn.drawWithCx dCx renderer (() <$ evTick)

  pure ()

type Coords = [((Integer, Integer), Pixel)]

data State = State
  { width :: Integer
  , height :: Integer
  , randomNum :: IO Word8
  , toDraw :: [((Integer, Integer), Pixel)]
  }

instance Show State where
  show (State w h _ [((x, y), _)]) = "State: " ++ show w ++ " " ++ show h ++ " " ++ show x ++  " " ++ show y

getSize w h rnd init gameTick =
  -- XXX Width and height have to be even numbers
  if gameTick > 0 then State (clamped w) (clamped h) rnd coords else State 2 2 rnd coords
  where clamped val = if gameTick * 2 < val then gameTick * 2 else val
        -- coords = [((0, 0), blackPixel)]
        coords = zip init [blackPixel]

inBounds :: (Ord b, Ord a, Num b, Num a) => (b, a) -> b -> a -> Bool
inBounds (x, y) w h = and [x >= 0, x <= w, y >= 0, y <= h]


x `floorDiv` y = (\(r, _) -> r) $ x `divMod` y

startingCoords :: Integer -> Integer -> IO Word8 -> IO [(Integer, Integer)]
startingCoords maxWidth maxHeight rnd = do
  startingX <- rnd
  startingY <- rnd
  let x = fromIntegral startingX
  let y = fromIntegral startingY
  if inBounds (x + bW, y + bH) (bound maxWidth) (bound maxHeight)
  then pure [(x + bW, y + bH)]
  else startingCoords maxWidth maxHeight rnd
  -- else pure [(0,0)]
  where bound num = num - (num `floorDiv` 4)
        bW = maxWidth `floorDiv` 8
        bH = maxWidth `floorDiv` 8
        mW = bound maxWidth
        mH = bound maxHeight

coordsToVisit :: Integer -> Integer -> IO Word8 -> IO [(Integer, Integer)]
coordsToVisit maxWidth maxHeight rnd = do
  init <- startingCoords maxWidth maxHeight rnd
  pure init

render :: MonadJSM m => State -> CanvasRenderingContext2D -> m ()
render st cx = do
  let (w, h, rnd) = (width st, height st, randomNum st)
  let [((x, y), pix)] = toDraw st
  -- growingSquare cx w h
  -- beamMeUp cx w h
  one <- liftIO $ rnd
  two <- liftIO $ rnd
  three <- liftIO $ rnd

  -- drawPixel cx whitePixel ((w-2)) 0
  -- drawPixel cx (mkPixel [one, two, three, 0xff]) (w) 0
  drawPixel cx pix (x * 2 + w) (y * 2 + h)
  pure ()


-- Drawing ---------------------------------------------------------------------

-- pixelSize has to be even, or you get a javascript exception, dunno why
pixelSize :: Integer
pixelSize = 2

mkPixel color = concat $ take (fromIntegral (pixelSize * pixelSize)) $ repeat color
pos x = x * pixelSize

blackPixel :: Pixel
blackPixel = mkPixel [0x00, 0x00, 0x00,0xff]
whitePixel = mkPixel [0xff, 0xff, 0xff,0xff]

drawPixel :: MonadJSM m => CanvasRenderingContext2D -> Pixel -> Integer -> Integer ->  m ()
drawPixel cx pixel xPos yPos = do
  img <- makeImageData pixelSize pixelSize pixel
  C.putImageData cx img (fromIntegral xPos) (fromIntegral yPos)

growingSquare :: MonadJSM m => CanvasRenderingContext2D -> Integer -> Integer ->  m ()
growingSquare cx w h = do
  let smallImage = concat $ take (fromIntegral (w * h)) $ repeat blackPixel
  img <- makeImageData w h smallImage
  C.putImageData cx img 0 0

beamMeUp :: MonadJSM m => CanvasRenderingContext2D -> Integer -> Integer ->  m ()
beamMeUp cx w h = do
  let smallImage = concat $ take (fromIntegral (w * h)) $ repeat blackPixel
  img <- makeImageData w h smallImage
  C.putImageData cx img (fromIntegral w) (fromIntegral h)
