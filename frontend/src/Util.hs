{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Util where

import qualified Data.Map as Map
import qualified Reflex.Dom as RD
import qualified Data.Text as T

import Control.Monad.Fix (MonadFix)

import JSDOM (currentWindowUnchecked)
import JSDOM.Types (Window)
import JSDOM.Generated.Window (getInnerWidth, getInnerHeight)
-- Used for style sheet embeding:
-- import Data.FileEmbed

-- Debugging -------------------------------------------------------------------

debug = True

tE e = tE' "" e

tE' :: (Monad m, RD.Reflex t, Show a)
    => T.Text -> m (RD.Event t a) -> m (RD.Event t a)
tE' s e =
  if debug then do
    event <- e
    let tracedEvent = RD.traceEvent (T.unpack s) event
    return tracedEvent
  else e

tEW :: (Monad m, RD.Reflex t)
    => T.Text -> m (RD.Event t a) -> m (RD.Event t a)
tEW s e =
  if debug then do
    event <- e
    let tracedEvent = RD.traceEventWith (\_ -> T.unpack s) event
    return tracedEvent
  else e

-- Util ------------------------------------------------------------------------

--     numbs <- (0 :: Int) <| [(+ 1) <$ evIncr,
--                             (+ (-1)) <$ evDecr,
--                             const 0 <$ evReset]
--       RD.el "div" $ RD.display numbs
-- | Event Map
(<|) :: (Control.Monad.Fix.MonadFix m, RD.MonadHold t m, RD.Reflex t)
     => a -> [RD.Event t (a -> a)] -> m (RD.Dynamic t a)
initialValue <| eventFns = do
  RD.foldDyn ($) initialValue $ RD.leftmost eventFns

mkHeadElement :: RD.MonadWidget t m => T.Text -> T.Text -> m ()
mkHeadElement title styleSheetPath = do
  RD.el "title" $ RD.text title
  styleSheet styleSheetPath
  where
    styleSheet link = RD.elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", link)
      ]) $ RD.blank
    -- Not used, but this is how you embed
    -- css = $(embedFile styleSheetPath)

screenSize :: RD.MonadWidget t m =>  m (Int, Int)
screenSize = do
  w <- currentWindowUnchecked
  width <- getInnerWidth w
  height <- getInnerHeight w
  return (width, height)
