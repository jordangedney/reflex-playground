{-# LANGUAGE OverloadedStrings     #-}
module Squares where

import Data.Text as Text
import Style (css)

import Language.Javascript.JSaddle.Warp as Warp

import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import qualified Reflex.Dom as RD
import qualified Reflex.Dom.CanvasBuilder.Types as CD
import qualified Reflex.Dom.CanvasDyn as CD
import Data.Map as Map
import JSDOM.Types as JSDOMTypes

infixl 0 |>
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

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

squaresApp :: RD.MonadWidget t m => m ()
squaresApp = do
  (topLevelWrapper,  dCx) <- createBlankCanvas

  RD.divClass "DEBUG" $
    RD.display "Hello"

-- main :: IO ()
-- main = Warp.run 3911 $ RD.mainWidget app
