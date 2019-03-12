module Main (main) where

import qualified Language.Javascript.JSaddle.Warp as JSW
import qualified Reflex.Dom.Core as RDC
import qualified Reflex.Dom as RD
import RandomFloodFill (headElement, bodyElement)
import Data.IORef (newIORef)

main :: IO ()
main = do
  box <- newIORef (4 :: Int)
  JSW.run 3911 $ RDC.mainWidgetWithHead headElement (bodyElement box)
-- main = RD.mainWidgetWithHead headElement bodyElement
