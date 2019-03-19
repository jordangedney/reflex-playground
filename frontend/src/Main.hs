module Main (main) where

import qualified Language.Javascript.JSaddle.Warp as JSW
import qualified Reflex.Dom.Core as RDC
import qualified Reflex.Dom as RD
import RandomFloodFill (headElement, bodyElement)
import System.Random

main :: IO ()
main = do
  let randomNum = randomIO :: IO Integer
  JSW.run 3911 $ RDC.mainWidgetWithHead headElement (bodyElement randomNum)
-- main = RD.mainWidgetWithHead headElement bodyElement
