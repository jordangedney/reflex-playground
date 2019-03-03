module Main (main) where

import qualified Language.Javascript.JSaddle.Warp as JSW
import qualified Reflex.Dom.Core as RDC
import qualified Reflex.Dom as RD
import RandomFloodFill (headElement, bodyElement)

main :: IO ()
main = JSW.run 3911 $ RDC.mainWidgetWithHead headElement bodyElement
-- main = RD.mainWidgetWithHead headElement bodyElement
