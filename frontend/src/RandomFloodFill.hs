{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE JavaScriptFFI #-}

module RandomFloodFill (headElement, bodyElement) where

---
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Base64 as B64 (encode)
import Language.Javascript.JSaddle (js, js1, jss, jsg, jsg1,
                                    new, pToJSVal, GHCJSPure(..), ghcjsPure, JSM,
                                    fromJSVal, toJSVal, Object, liftJSM)

import JSDOM.Types (liftDOM, Uint8ClampedArray(..), RenderingContext(..))
import JSDOM.ImageData
import JSDOM.HTMLCanvasElement
import JSDOM.CanvasRenderingContext2D
import GHCJS.Buffer (getArrayBuffer, MutableBuffer)
import GHCJS.Buffer.Types (SomeBuffer(..))
import Control.Lens ((^.))

import Util

-- Qualified -------------------------------------------------------------------

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Monoid

import qualified Reflex.Dom as RD
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified Reflex.Dom.CanvasDyn as CDyn
import qualified JSDOM.CanvasRenderingContext2D as C
import qualified JSDOM.ImageData as ID

-- Direct ----------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Time (getCurrentTime)
import Control.Monad.Trans (liftIO)
import Data.Monoid ((<>))
import Foreign.Ptr (Ptr)

import JSDOM.Types (
  JSString,
  MonadJSM,
  unUint8ClampedArray,
  Uint8ClampedArray,
  JSVal,
  )
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import Reflex.Dom ((=:))

--------------------------------------------------------------------------------

headElement :: RD.MonadWidget t m => m ()
headElement = mkHeadElement "Random Flood Fill" "css/simple.css"

bodyElement :: RD.MonadWidget t m => m ()
bodyElement = do
  (width, height) <- screenSize
  evStart <- RD.getPostBuild
  now <- liftIO getCurrentTime

  -- evTick <- tE $ RD.tickLossy 1 now
  evTick <- RD.tickLossy 1 now

  dCx <- createBlankCanvas $
          (canvasAttrs width height) <>
          ("imageRendering" =: "pixelated")

  evRendered <- CDyn.drawWithCx dCx ((\cx _ _ -> render cx) <$> dCx) (() <$ evTick)

  pure ()

render :: MonadJSM m => CanvasRenderingContext2D -> m ()
render cx = do
  -- t <- C.getImageData cx 0 0 0 0
  -- x <- ID.getData t
  -- let y = unUint8ClampedArray x

  let smallImage = BS.pack [0xff,0x00,0x00,0xff,  0xff,0x00,0x00,0xff,  0xff,0x00,0x00,0xff,
                            0x00,0x00,0x00,0xff,  0x00,0xff,0x00,0xff,  0x00,0x00,0x00,0xff,
                            0x00,0x00,0xff,0xff,  0x00,0x00,0xff,0xff,  0x00,0x00,0xff,0xff,
                            0x00,0x00,0xff,0xff,  0x00,0x00,0x00,0xff,  0x00,0x00,0xff,0xff]
  img <- liftJSM $ makeImageData 3 4 smallImage
  t' <- C.putImageData cx img 3 4

  -- C.setFillStyle cx ("blue" :: JSString)
  -- C.fillRect cx 50  50 10 10
  -- C.setStrokeStyle cx ("black" :: JSString)
  -- C.strokeRect cx 50 50 10 10
  pure ()

uint8ClampedArrayFromByteString :: ByteString -> GHCJSPure (Uint8ClampedArray)
uint8ClampedArrayFromByteString bs = GHCJSPure $ do
  buffer <- SomeBuffer <$> jsg1 (T.pack "h$newByteArrayFromBase64String")
                                (decodeUtf8 $ B64.encode bs)
  arrbuff <- ghcjsPure (getArrayBuffer (buffer :: MutableBuffer))
  liftDOM (Uint8ClampedArray <$> new (jsg (T.pack "Uint8ClampedArray")) [pToJSVal arrbuff])

makeImageData :: Int -> Int -> ByteString -> JSM ImageData
makeImageData width height dat
  = do dat' <- ghcjsPure (uint8ClampedArrayFromByteString dat)
       newImageData dat' (fromIntegral width) (Just (fromIntegral height))