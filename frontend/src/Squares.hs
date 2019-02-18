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
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified Reflex.Dom.CanvasDyn as CD
import Data.Map as Map
import JSDOM.Types as JSDOMTypes

infixl 0 |>
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

data SqType
  = Wall
  | Floor
  | Active
  deriving (Generic, NFData, Eq,Show)

data Sqr = Sqr
  { _sqType :: SqType
  , _sqSide :: Int
  }
  deriving (Eq, Generic, NFData, Show)

newtype Room = Room
  { unRoom :: [[Sqr]]
  }

sqrSize :: Int
sqrSize = 10

sqrSizeF :: Float
sqrSizeF = fromIntegral sqrSize

room1 :: Room
room1 = Room [ [w,w,w,w,w,w,w,w]
             , [w,f,w,f,f,f,f,w]
             , [w,f,f,f,f,w,f,w]
             , [w,f,f,a,f,w,f,w]
             , [w,f,f,f,f,f,f,w]
             , [w,f,f,f,w,w,w,w]
             , [w,w,f,f,f,f,f,w]
             , [w,w,w,w,w,w,w,w]
             ]
  where w = Sqr Wall sqrSize
        f = Sqr Floor sqrSize
        a = Sqr Active sqrSize

sqColour :: Sqr -> JSString
sqColour sq = case _sqType sq of
  Wall -> "darkgrey"
  Floor -> "aqua"
  _ -> "white"

renderSqr
  :: CanvasRenderingContext2D
  -> V2 Float
  -> Sqr
  -> JSM ()
renderSqr cx pos sq = do
  C.setFillStyle cx $ sqColour sq
  C.fillRect cx xPos yPos sqrSizeF sqrSizeF
  C.setStrokeStyle cx ("black" :: JSString)
  C.strokeRect cx xPos yPos sqrSizeF sqrSizeF
  where
    pos' getter = pos ^. getter . to realToFrac
    xPos = pos' _x
    yPos = pos' _y

-- width (Room [rms]) = Prelude.length rms * sqrSize
width :: Room -> Int
width (Room []) = 10
width (Room x) = Prelude.length (x !! 0) * sqrSize

screenWidth :: Float
-- screenWidth = 320
-- screenWidth = 640
screenWidth = fromIntegral $ width room1

height (Room rm) = Prelude.length rm * sqrSize

screenHeight :: Float
-- screenHeight = 240
-- screenHeight = 480
screenHeight = fromIntegral $ height room1

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
     f1 (f (CBT.CanvasInfo c t)) -> f1 (f (CBT.RenderContext c))
canvasInfoToRenderContext = (fmap . fmap) CBT._canvasInfo_context

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

  let emptyConfig = CBT.CanvasConfig innerEle mempty
      innerCanvasInfo = CD.dContext2d emptyConfig

  dCx <- innerCanvasInfo |> canvasInfoToRenderContext
  return (wrapperEle, dCx)

renderRoom :: Room -> CanvasRenderingContext2D -> JSM ()
renderRoom (Room roomTiles) cx =
  let
    initialPos = V2 0.0 0.0
    xStep = V2 sqrSizeF 0.0
    yStep = V2 0.0 sqrSizeF

    renderSqr' :: Sqr -> V2 Float -> JSM (V2 Float)
    renderSqr' sq offset = renderedSquare $> offset
      where renderedSquare = renderSqr cx offset sq

    test pos square = (xStep +) <$> renderSqr' square pos

    foo pos sqs = yStep + pos <$ foldlM test pos sqs
  in
    void $ foldlM foo initialPos roomTiles

-- >>> :t ($>)
-- ($>) :: Functor f => f a -> b -> f b

-- >>> :t (<$)
-- (<$) :: Functor f => a -> f b -> f a

renderMap cx _ _ = do
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

  eRendered <- CD.drawWithCx dCx (renderMap <$> dCx) eDraw

  pure ()

  -- RD.divClass "DEBUG" $
  --   RD.display "Hello"

-- main :: IO ()
-- main = Warp.run 3911 $ RD.mainWidget app
