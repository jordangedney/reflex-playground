
{-# LANGUAGE FlexibleContexts #-}
module JSUtil where

import Data.ByteString (ByteString)
import Data.Word
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
import qualified Data.Text as T

import qualified Reflex as R
import qualified Reflex.Dom as RD
import qualified Reflex.Dom as RD ((<@))
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified GHCJS.DOM as JSDOM

-- https://stackoverflow.com/questions/43571803/how-to-convert-a-bytestring-value-to-a-jsval
uint8ClampedArrayFromByteString :: ByteString -> GHCJSPure (Uint8ClampedArray)
uint8ClampedArrayFromByteString bs = GHCJSPure $ do
  buffer <- SomeBuffer <$> jsg1 (T.pack "h$newByteArrayFromBase64String")
                                (decodeUtf8 $ B64.encode bs)
  arrbuff <- ghcjsPure (getArrayBuffer (buffer :: MutableBuffer))
  liftDOM (Uint8ClampedArray <$> new (jsg (T.pack "Uint8ClampedArray")) [pToJSVal arrbuff])

makeImageData' :: Int -> Int -> [Word8] -> JSM ImageData
makeImageData' width height dat
  = do dat' <- ghcjsPure (uint8ClampedArrayFromByteString (BS.pack dat))
       newImageData dat' (fromIntegral width) (Just (fromIntegral height))

makeImageData x y z = do
  x' <- liftJSM $ makeImageData' x y z
  return x'

-- (RD.<@)
--   :: RD.Reflex t => RD.Behavior t b -> RD.Event t a -> RD.Event t b

drawWithCx'
  :: ( RD.MonadWidget t m
     , CBT.HasRenderFn c ( CBT.RenderContext c )
     )
  => RD.Dynamic t ( CBT.RenderContext c )
  -> RD.Dynamic t ( CBT.RenderContext c -> Double -> JSM a )
  -> RD.Event t ()
  -> m ( RD.Event t a )
drawWithCx' dContext dAction eApply =
  let
    nextFrame cx f = liftJSM $
      JSDOM.nextAnimationFrame (f cx)
    huh = ( nextFrame
            <$> R.current dContext
            <*> R.current dAction
            RD.<@ eApply )
  in
    RD.performEvent huh