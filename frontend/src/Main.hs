{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
module Main (main) where

import Control.Applicative ((<|>))
import Control.Lens (to, (+~), (.~), (^.), (%~), imap, _Wrapped)
import Control.Monad (void)
import Control.Monad.State (MonadState, get, put)
import Data.Foldable (foldlM, traverse_)
import Data.Function ((&))
import Data.Functor (($>), (<$))
import Data.Maybe (catMaybes,mapMaybe)
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Map (Map)
import Linear.Matrix (identity, M44, (!*))
import Linear.V4 (_w)
import Linear.V2 (V2 (..), _x, _y)

import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom (MonadWidget, (=:))
import Reflex.Dom.Core (mainWidget, mainWidgetWithCss)
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import JSDOM.Types (JSM, JSString, liftJSM)

import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.EventM (on, preventDefault)
import GHCJS.DOM.GlobalEventHandlers (keyDown)
import Web.KeyCode

import Squares (squaresApp)
import DrawM (DrawM, runDrawM)
import Style (css)
import RayCaster (castSingleRay)
import Types (Angle (..), FOV (..), Height (..), Ray (Ray), RayBeta (RayBeta), RayCast,
              Room (..), SqType (..), Sqr (..), Width (..))

import qualified Data.List as L
import qualified Reflex as R
import qualified Reflex.Dom as RD
import qualified Data.Map as Map
import qualified JSDOM.CanvasRenderingContext2D as C
import qualified JSDOM.CanvasPath as C
import qualified Reflex.Dom.CanvasBuilder.Types as CD
import qualified Reflex.Dom.CanvasDyn as CD
import qualified RayCaster as R
import qualified RayWut as R
import qualified Types as T

infixl 0 |>
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

screenWidth :: Double
screenWidth = 320

screenHeight :: Double
screenHeight = 240

sqrSize :: Int
sqrSize = 64

fovAngle :: Double
fovAngle = 60

playerSpeed :: Double
playerSpeed = 10

angleBetweenRays :: Double
angleBetweenRays = fovAngle / screenWidth

halfFOV :: T.Angle
halfFOV = T.Angle (fovAngle / 2)

rayList :: [Double]
rayList = (angleBetweenRays *) <$> [0.. (screenWidth - 1)]

betaRays :: [RayBeta]
betaRays =
  let
    halfFOV' = _unAngle halfFOV
    step = halfFOV' - angleBetweenRays
  in
    [ RayBeta rb | rb <- [halfFOV', step .. (negate halfFOV')] ]

player :: T.Player
player = T.Player (V2 p (p - s)) (Angle fovAngle)
  where
    s = fromIntegral sqrSize
    p = s * 3 + 32

fov :: T.FOV
fov = R.mkFov
  (Height (floor screenHeight))
  (Width (floor screenWidth))
  (Angle fovAngle)

mapCanvasAttrs :: Map Text Text
mapCanvasAttrs = Map.fromList
  [ ("width", "512")
  , ("height", "512")
  ]

cameraAttrs :: Map Text Text
cameraAttrs = Map.fromList
  [ ("width", pack . show $ screenWidth)
  , ("height", pack . show $ screenHeight)
  -- , ("style", "transform-origin: 0 0;transform: scale(1.5, 1.5);")
  ]

calculateRayCast
  :: (Fractional t, Integral t1) =>
     FOV -> Int -> t1 -> RayCast -> (t, Double, Double)
calculateRayCast fov slice rIth rc = (x, y, sliceHeight)
  where width = fov ^. T.fovWidth . _Wrapped . to fromIntegral
        height = fov ^. T.fovHeight . _Wrapped . to fromIntegral
        sliceHeight = R.projectedSliceHeight slice rc fov
        x = (negate width/2) + (fromIntegral rIth)
        y = negate (sliceHeight/2)

renderRayCast' cx x y sliceHeight = do
  C.moveTo cx (realToFrac x) (realToFrac y)
  C.lineTo cx (realToFrac x) (realToFrac $ y + sliceHeight)

renderRayCast
  :: CanvasRenderingContext2D
  -> FOV
  -> Int
  -> Int
  -> RayCast
  -> JSM ()
renderRayCast cx fov slice rIth rc = do
  let (x, y, sliceHeight) = calculateRayCast fov slice rIth rc
  renderRayCast' cx x y sliceHeight

stepSquares
  :: Room
  -> Int
  -> (V2 Int -> Int -> Ray -> V2 Int)
  -> Ray
  -> V2 Int
  -> Maybe ((V2 Int,Sqr), V2 Int)
stepSquares rm sqrS fn ray v@(V2 x y) =
  (\s -> ((v,s), fn v sqrS ray)) <$> T.atPos (R.toRoomCoord sqrS x y) rm

mkHorizInters, mkVertInters :: Ray -> T.Player -> [(V2 Int, T.Sqr)]
mkHorizInters r p =
  let (T.HorizAX hx, T.HorizAY hy) = R.firstHorizontalIntersection r p sqrSize
  in L.unfoldr (stepSquares T.room1 sqrSize R.stepHorizontalIntersection r) (V2 hx hy)
mkVertInters r p =
  let (T.VertAX vx, T.VertAY vy) = R.firstVerticalIntersection r p sqrSize
  in L.unfoldr (stepSquares T.room1 sqrSize R.stepVerticalIntersection r) (V2 vx vy)

drawInters
  :: CanvasRenderingContext2D
  -> V2 Int
  -> JSM ()
drawInters cx (V2 x y) =
  C.fillRect cx (fromIntegral x) (fromIntegral y) 10 10

renderMap  :: T.Player -> Ray -> Ray -> CanvasRenderingContext2D -> t -> JSM ()
renderMap p firstRay lastRay cx _ = do
  renderRoom T.room1 cx
  renderPlayer p cx

  C.setFillStyle cx ("blue" :: JSString)
  traverse_ (drawInters cx . fst) (mkHorizInters firstRay p)
  C.setFillStyle cx ("green" :: JSString)
  traverse_ (drawInters cx . fst) (mkVertInters firstRay p)

  C.setFillStyle cx ("blue" :: JSString)
  traverse_ (drawInters cx . fst) (mkHorizInters lastRay p)
  C.setFillStyle cx ("green" :: JSString)
  traverse_ (drawInters cx . fst) (mkVertInters lastRay p)

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

renderPlayer :: T.Player -> CanvasRenderingContext2D -> JSM ()
renderPlayer (T.Player pos face) cx = do
  C.save cx

  C.translate cx (pos ^. _x . to realToFrac) (pos ^. _y . to realToFrac)
  let
    faceRadians = realToFrac $ T.toRadians face
    rCos = cos faceRadians
    rSin = sin faceRadians

  C.rotate cx (-faceRadians)

  C.setStrokeStyle cx ("red" :: JSString)
  C.strokeRect cx 0 0 10 10

  C.setLineWidth cx 5

  C.beginPath cx
  C.moveTo cx 0 0
  C.lineTo cx 100 0
  C.closePath cx
  C.stroke cx
  C.restore cx

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


--------------------------------------------------------------------------------------------

blankCanvas
  :: MonadWidget t m => Map Text Text
                     -> m (RD.Element RD.EventResult (RD.DomBuilderSpace m) t, ())
blankCanvas attributes = RD.elAttr' "canvas" attributes RD.blank

tableDiv
  :: MonadWidget t m => m a
                     -> m (RD.Element RD.EventResult (RD.DomBuilderSpace m) t, a)
tableDiv = RD.elAttr' "div" ("tabindex" =: "0")

canvasInfoToRenderContext
  :: (Functor f1, Functor f) =>
     f1 (f (CD.CanvasInfo c t)) -> f1 (f (CD.RenderContext c))
canvasInfoToRenderContext = (fmap . fmap) CD._canvasInfo_context

rayCast :: T.Player -> Int -> Double -> (RayCast, Int)
rayCast p rIth rayAngle =
  let
    rA = p ^. T.playerFacing . to (R.createRay halfFOV rayAngle)

    -- where RayBeta is the angle of the ray that is being cast relative to
    -- the viewing angle. I can get away with this because we generate the
    -- list of betaRays beforehand and we have a known quantity based on the `rayList`
    rB = betaRays !! rIth
  in
    (castSingleRay T.room1 sqrSize rA rB p, rIth)

createRayTracingCanvases
  :: MonadWidget t m =>
   m (RD.Element RD.EventResult (RD.DomBuilderSpace m) t,
       RD.Dynamic t CanvasRenderingContext2D,
       RD.Dynamic t CanvasRenderingContext2D)
createRayTracingCanvases = do
  (wrapperEle, (innerEle, _)) <- blankCanvas cameraAttrs |> tableDiv
  (e, _) <- blankCanvas mapCanvasAttrs

  let
    emptyConfig element = CD.CanvasConfig element mempty
    cameraCanvasConfig = emptyConfig innerEle
    innerCanvasInfo = CD.dContext2d cameraCanvasConfig
    mapCanvasConfig = emptyConfig e
    mainCanvasInfo = CD.dContext2d mapCanvasConfig

  dCamCx <- innerCanvasInfo |> canvasInfoToRenderContext
  dCx <- mainCanvasInfo |> canvasInfoToRenderContext
  return (wrapperEle, dCamCx, dCx)

buildRays :: T.Player -> [(RayCast, Int)]
buildRays ply = rayCast ply `imap` rayList

rays
  :: Foldable t =>
     CanvasRenderingContext2D -> t (RayCast, Int) -> JSM ()
rays cx rs = do
  C.beginPath cx
  traverse_ (\(rc,i) -> renderRayCast cx fov sqrSize i rc) rs
  C.closePath cx
  C.stroke cx

camRender
  :: [(RayCast, Int)] -> CanvasRenderingContext2D -> Double -> JSM ()
camRender r cx _ = do
  C.clearRect cx 0 0 (realToFrac screenWidth) (realToFrac screenHeight)
  C.save cx
  C.setFillStyle cx ("lightblue" :: JSString)
  C.fillRect cx 0 0 (realToFrac screenWidth) (realToFrac screenHeight)
  C.setFillStyle cx ("darkgrey" :: JSString)
  C.translate cx (realToFrac $ screenWidth / 2) (realToFrac $ screenHeight / 2)
  rays cx r
  C.restore cx

-- Rotation based on Ray Casting 0 at E, 90 at N, 180 at W, 270 at S

rotRight a = T.playerFacing %~ (`T.subtractAngle` a)
rotLeft a = T.playerFacing %~ T.addAngle a

moveForward :: T.Player -> T.Player
moveForward p =
  R.movePlayer p T.Forward playerSpeed

moveBackward :: T.Player -> T.Player
moveBackward p =
  R.movePlayer p T.Backward playerSpeed

firstAndLastRay :: Functor f => f T.Player -> (f Ray, f Ray)
firstAndLastRay player = (firstRay, lastRay)
  where firstRay =
          (^. T.playerFacing . to (Ray . T._unAngle . (`T.subtractAngle` halfFOV)) )
          <$> player
        lastRay =
          (^. T.playerFacing . to (Ray . T._unAngle . T.addAngle halfFOV) )
          <$> player

getKeyPress = do
    preventDefault -- This 'preventDefault' is here to prevent
                   -- the browser's default behavior when keys
                   -- like <F1> or the arrow keys are pressed.
                   -- If you want to preserve default behavior
                   -- this can be removed, or you can apply it
                   -- selectively, only to certain keypresses.
    i <- RD.getKeyEvent
    return i

app :: MonadWidget t m => m ()
app = do
  (topLevelWrapper, dCamCx, dCx) <- createRayTracingCanvases

  eDraw <- RD.button "Go"
  eLeftBtn <- RD.button "Turn left a bit"
  eRightBtn <- RD.button "Turn right a bit"
  eForwardBtn <- RD.button "Go forward"
  eBackwardBtn <- RD.button "Go backward"

  doc <- RD.askDocument
  Just body <- getBody doc
  kp <- RD.wrapDomEvent body (`on` keyDown) getKeyPress

  let keyPressed =  Just . keyCodeLookup . fromEnum <$> kp
  lastKeyPressed <- RD.holdDyn Nothing keyPressed

  let
    check = (\x -> RD.keypress x topLevelWrapper)
    [leftPressed, rightPressed, forwardPressed, backwardPressed] =
      map check [RD.ArrowLeft, RD.ArrowRight, RD.ArrowUp, RD.ArrowDown]

    eLeft = check RD.ArrowLeft <> eLeftBtn
    eRight = RD.keypress RD.ArrowRight topLevelWrapper <> eRightBtn
    eForward = RD.keypress RD.ArrowUp topLevelWrapper <> eForwardBtn
    eBackward = RD.keypress RD.ArrowDown topLevelWrapper <> eBackwardBtn

    actions =
      [ rotLeft (Angle 2) <$ eLeft
      , rotRight (Angle 2) <$ eRight
      , moveForward <$ eForward
      , moveBackward <$ eBackward
      ]

  dPlayer <- R.foldDyn ($) player $ R.mergeWith (.) actions

  let
    (dFirstRay, dLastRay) = firstAndLastRay dPlayer
    builtRays = buildRays player

  dRays <- buildRays <$> R.updated dPlayer |> R.holdDyn builtRays

  let
    eMoved = eDraw <> (() <$ R.updated dPlayer)
    renderer = (renderMap <$> dPlayer <*> dFirstRay <*> dLastRay)

  eRendered <- CD.drawWithCx dCx renderer eMoved
  eRendered' <- CD.drawWithCx dCamCx (camRender <$> dRays) eMoved

  dRendered <- R.holdDyn "Not Rendered" $
    "Rendered!" <$ eRendered

  RD.divClass "DEBUG" $
    RD.display dRendered

  RD.display lastKeyPressed

main :: IO ()
main = run 3911 $ mainWidgetWithCss css squaresApp
-- main = run 3911 $ mainWidget app
