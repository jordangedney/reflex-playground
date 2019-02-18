{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Squares where

import Control.Monad (void)
import Data.Functor (($>), (<$))
import Control.Lens (to, (+~), (.~), (^.), (%~), imap, _Wrapped)
import Data.Foldable (foldlM, traverse_)
import Linear.V2 (V2 (..), _x, _y)
import GHC.Generics (Generic)
import Control.Monad.Par (NFData)
import GHC.Word (Word8)
import Data.Text as Text
import Style (css)

import Language.Javascript.JSaddle.Warp as Warp

import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)

import qualified JSDOM.CanvasRenderingContext2D as C
import qualified Reflex.Dom as RD
import qualified Reflex.Dom.CanvasBuilder.Types as CD
import qualified Reflex.Dom.CanvasDyn as CD
import Data.Map as Map
import JSDOM.Types as JSDOMTypes

infixl 0 |>
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

data SqType
  = Wall
  | Floor
  deriving (Generic, NFData, Eq,Show)

data Sqr = Sqr
  { _sqType :: SqType
  , _sqSide :: Word8
  }
  deriving (Eq, Generic, NFData, Show)

newtype Room = Room
  { unRoom :: [[Sqr]]
  }

room1 :: Room
room1 = Room [ [w,w,w,w,w,w,w,w]
             , [w,f,w,f,f,f,f,w]
             , [w,f,f,f,f,w,f,w]
             , [w,f,f,f,f,w,f,w]
             , [w,f,f,f,f,f,f,w]
             , [w,f,f,f,w,w,w,w]
             , [w,w,f,f,f,f,f,w]
             , [w,w,w,w,w,w,w,w]
             ]
  where w = Sqr Wall 64
        f = Sqr Floor 64

renderSqr
  :: V2 Double
  -> Sqr
  -> CanvasRenderingContext2D
  -> JSM ()
renderSqr pos sq cx = do
  C.setFillStyle cx sqColour
  C.fillRect cx (p _x) (p _y) sqS sqS
  C.setStrokeStyle cx ("black" :: JSString)
  C.strokeRect cx (p _x) (p _y) sqS sqS
  where
    sqS = fromIntegral sqrSize
    p l = pos ^. l . to realToFrac

    sqColour :: JSString
    sqColour = case _sqType sq of
      Wall -> "darkgrey"
      Floor -> "aqua"

screenWidth :: Double
screenWidth = 320

screenHeight :: Double
screenHeight = 240

sqrSize :: Int
sqrSize = 64

emptyDiv :: Map.Map Text.Text Text.Text
emptyDiv = Map.fromList []

canvasAttrs :: Map.Map Text.Text Text.Text
canvasAttrs = Map.fromList
  [ ("width", pack . show $ screenWidth)
  , ("height", pack . show $ screenHeight)
  -- , ("style", "transform-origin: 0 0;transform: scale(1.5, 1.5);")
  ]

canvasInfoToRenderContext
  :: (Functor f1, Functor f) =>
     f1 (f (CD.CanvasInfo c t)) -> f1 (f (CD.RenderContext c))
canvasInfoToRenderContext = (fmap . fmap) CD._canvasInfo_context

blankCanvas
  :: RD.MonadWidget t m
  => Map Text.Text Text.Text
  -> m (RD.Element RD.EventResult (RD.DomBuilderSpace m) t, ())
blankCanvas attributes = RD.elAttr' "canvas" attributes RD.blank

tableDiv
  :: RD.MonadWidget t m
  => m a
  -> m (RD.Element RD.EventResult (RD.DomBuilderSpace m) t, a)
tableDiv = RD.elAttr' "div" emptyDiv

createBlankCanvas
  :: RD.MonadWidget t m
  => m (RD.Element RD.EventResult (RD.DomBuilderSpace m) t,
        RD.Dynamic t CanvasRenderingContext2D)
createBlankCanvas = do
  (wrapperEle, (innerEle, _)) <- blankCanvas canvasAttrs |> tableDiv

  let emptyConfig = CD.CanvasConfig innerEle mempty
      innerCanvasInfo = CD.dContext2d emptyConfig

  dCx <- innerCanvasInfo |> canvasInfoToRenderContext
  return (wrapperEle, dCx)

renderRoom :: Room -> CanvasRenderingContext2D -> JSM ()
renderRoom (Room rm) cx =
  let
    o = V2 0.0 0.0
    xStep = V2 (fromIntegral sqrSize) 0.0
    yStep = V2 0.0  (fromIntegral sqrSize)

    rSqrs sq offs =
      renderSqr offs sq cx $> offs
  in
    --- Hrmmm
    void $ foldlM
      (\o' sqs ->
          yStep + o' <$ foldlM
          (\o'' s ->
              (xStep +) <$> rSqrs s o''
          ) o' sqs
      ) o rm

renderMap  :: CanvasRenderingContext2D -> t -> JSM ()
renderMap cx _ = do
  renderRoom room1 cx
  -- renderPlayer p cx

  -- C.setFillStyle cx ("blue" :: JSString)
  -- traverse_ (drawInters cx . fst) (mkHorizInters firstRay p)
  -- C.setFillStyle cx ("green" :: JSString)
  -- traverse_ (drawInters cx . fst) (mkVertInters firstRay p)

  -- C.setFillStyle cx ("blue" :: JSString)
  -- traverse_ (drawInters cx . fst) (mkHorizInters lastRay p)
  -- C.setFillStyle cx ("green" :: JSString)
  -- traverse_ (drawInters cx . fst) (mkVertInters lastRay p)

squaresApp :: RD.MonadWidget t m => m ()
squaresApp = do
  (topLevelWrapper,  dCx) <- createBlankCanvas

  eDraw <- RD.button "Go"
  --renderer = (renderMap <$> dPlayer <*> dFirstRay <*> dLastRay)

  eRendered <- CD.drawWithCx dCx renderMap eDraw
  -- eRendered <- CD.drawWithCx dCx renderer eMoved
  RD.divClass "DEBUG" $
    RD.display "Hello"

-- main :: IO ()
-- main = Warp.run 3911 $ RD.mainWidget app
