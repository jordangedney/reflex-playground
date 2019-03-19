module Main (main) where

import qualified Language.Javascript.JSaddle.Warp as JSW
import qualified Reflex.Dom.Core as RDC
import qualified Reflex.Dom as RD
import RandomFloodFill (headElement, bodyElement)
import System.Random

-- ------------
import Language.Javascript.JSaddle            (JSM)
import Language.Javascript.JSaddle.Run        (syncPoint)
import Language.Javascript.JSaddle.WebSockets (debugWrapper, jsaddleWithAppOr)
import Network.Wai                            (Application)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettings, setPort, setTimeout)
import Network.WebSockets                     (defaultConnectionOptions)
import Data.Monoid ((<>))

main :: IO ()
main = do
  let randomNum = randomIO :: IO Integer
  JSW.run 3911 $ RDC.mainWidgetWithHead headElement (bodyElement randomNum)
-- main = RD.mainWidgetWithHead headElement bodyElement


-- | A @main@ for doing development.
devMain :: Application -> JSM () -> Int -> IO ()
devMain backend frontend port = do
  putStrLn $ "Running dev server on localhost:" <> show port

  app <- jsaddleWithAppOr
    defaultConnectionOptions
    (frontend >> syncPoint)
    backend

  runSettings (defaultSettings RD.& setTimeout 3600 RD.& setPort port) app


-- | A version of @devMain@ that can be used with @ghcid --test@ to get an auto-reloading server.
devMainAutoReload :: Application -> JSM () -> Int -> IO ()
devMainAutoReload backend frontend port =
  debugWrapper $ \refreshMiddleware registerContext ->
    devMain (refreshMiddleware backend) (registerContext >> frontend) port
