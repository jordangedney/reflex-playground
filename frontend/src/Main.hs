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

import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom (MonadWidget, (=:))
import Reflex.Dom.Core (mainWidget, mainWidgetWithCss, mainWidgetWithHead)
import JSDOM.CanvasRenderingContext2D (CanvasRenderingContext2D)
import JSDOM.Types (JSM, JSString, liftJSM)

import qualified RandomFloodFill as RFF
import Squares (squaresApp)
import DrawM (DrawM, runDrawM)
import Style (css, blackBkground)

import qualified Reflex.Dom as RD
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.FileEmbed
import Control.Monad.Fix (MonadFix)


--  https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md
-- Text Input Fields


main :: IO ()
main = run 3911 $ mainWidgetWithHead headElement bodyElement

headElement :: MonadWidget t m => m ()
headElement = do
  RD.el "title" $ RD.text "Main Title"
  styleSheet "css/simple.css"
  where
    styleSheet link = RD.elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ return ()
    css = $(embedFile "css/simple.css")

-- | Event Map
(<|) :: (Control.Monad.Fix.MonadFix m, RD.MonadHold t m, RD.Reflex t)
     => a -> [RD.Event t (a -> a)] -> m (RD.Dynamic t a)
initialValue <| eventFns = do
  RD.foldDyn ($) initialValue $ RD.leftmost eventFns

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  rec RD.el "h2" $ RD.text "Counter as a fold"
      numbs <- (0 :: Int) <| [(+ 1) <$ evIncr,
                              (+ (-1)) <$ evDecr,
                              const 0 <$ evReset]
      RD.el "div" $ RD.display numbs
      evIncr <- RD.button "Increment"
      evDecr  <- RD.button "Dec"
      evReset  <- RD.button "Reset"
  return ()