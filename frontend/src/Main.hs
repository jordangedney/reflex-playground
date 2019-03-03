{-# LANGUAGE
    OverloadedStrings,
    RecursiveDo,
    ScopedTypeVariables,
    FlexibleContexts,
    TypeFamilies,
    ConstraintKinds,
    TemplateHaskell
#-}
module Main (main) where

import qualified Language.Javascript.JSaddle.Warp as JSW
import Reflex.Dom (MonadWidget, (=:), (&), (.~))
import qualified Reflex.Dom.Core as RDC
-- (mainWidget, mainWidgetWithCss, mainWidgetWithHead)
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import JSDOM.Types (JSM, JSString, liftJSM)

import qualified RandomFloodFill as RFF
import Squares (squaresApp)
import DrawM (DrawM, runDrawM)
import Style (css, blackBkground)

import Reflex.Dom
import qualified Reflex.Dom as RD
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.FileEmbed
import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromJust, fromMaybe)
import Data.Time
import Control.Monad.Trans (liftIO)

main :: IO ()
main = JSW.run 3911 $ RDC.mainWidgetWithHead headElement bodyElement
-- main = mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  RD.el "title" $ RD.text "Playground"
  styleSheet "css/simple.css"
  where
    styleSheet link = RD.elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()
    css = $(embedFile "css/simple.css")

--     numbs <- (0 :: Int) <| [(+ 1) <$ evIncr,
--                             (+ (-1)) <$ evDecr,
--                             const 0 <$ evReset]
--       RD.el "div" $ RD.display numbs
-- | Event Map
(<|) :: (Control.Monad.Fix.MonadFix m, RD.MonadHold t m, RD.Reflex t)
     => a -> [RD.Event t (a -> a)] -> m (RD.Dynamic t a)
initialValue <| eventFns = do
  RD.foldDyn ($) initialValue $ RD.leftmost eventFns

debug = True

tE e = tE' "" e

tE' :: (Monad m, Reflex t, Show a)
    => T.Text -> m (Event t a) -> m (Event t a)
tE' s e =
  if debug then do
    event <- e
    let tracedEvent = traceEvent (T.unpack s) event
    return tracedEvent
  else e

tEW :: (Monad m, Reflex t)
    => T.Text -> m (Event t a) -> m (Event t a)
tEW s e =
  if debug then do
    event <- e
    let tracedEvent = traceEventWith (\_ -> T.unpack s) event
    return tracedEvent
  else e

bodyElement :: MonadWidget t m => m ()
bodyElement  = el "div" $ do
  el "h2" $ text "Swiss Meteo Data (raw version)"
  text "Choose station: "
  dd <- dropdown "BER" (constDyn stations) def
  -- Build and send the request
  evStart <- tE getPostBuild
  let evCode = tagPromptlyDyn (value dd) $
        leftmost [ () <$ (tE _dropdown_change dd), evStart]
  evRsp <- tEW "Received query result" (performRequestAsync $ buildReq <$> evCode)
  -- Display the whole response
  el "h5" $ text "Response Text:"
  let evResult = (fromMaybe "" . _xhrResponse_responseText) <$> evRsp
  dynText =<< holdDyn "" evResult
  return ()

buildReq :: T.Text -> XhrRequest ()
buildReq code = XhrRequest "GET" ("https://opendata.netcetera.com/smn/smn/" <> code) def

stations :: Map.Map T.Text T.Text
stations = Map.fromList [("BIN", "Binn"), ("BER", "Bern"), ("KLO", "Zurich airport"), ("ZER", "Zermatt"), ("JUN", "Jungfraujoch"), ("ARB", "ANN ARBOR")]