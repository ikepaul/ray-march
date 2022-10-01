module Main where

import Data.IORef
import Draw
import Graphics.UI.Threepenny hiding (map)
import Shapes

main :: IO ()
main = startGUI defaultConfig initGame

initGame :: Window -> UI ()
initGame window = do
  pure window # set title "testing"

  canvas <- canvas # set height (round canvasHeight) # set width (round canvasWidth)

  on mousedown canvas $ \(x, y) -> do
    let myCircle = Circle x y 30 (RGB 188 0 188)
    drawShape canvas myCircle

  mousePosRef <- liftIO $ newIORef ((0, 0) :: Point)

  on mousemove canvas $ \(x, y) -> do
    liftIO $ writeIORef mousePosRef (x, y)
    gameLoop window canvas myShapes mousePosRef

  body <- getBody window
  pure body
    #+ [column [element canvas]]

  timer1 <- set interval 3000 timer
  start timer1

  on tick timer1 $ \() -> gameLoop window canvas myShapes mousePosRef

  timer2 <- set interval 3000 timer
  start timer2
  on tick timer2 $ \() -> do
    return ()
  --let myRectangle = Rectangle 200 200 30 100 (RGB 0 100 100)
  --drawShape canvas myRectangle

  --gameLoop window canvas

  return ()

gameLoop :: Window -> Element -> [Shape] -> IORef Point -> UI ()
gameLoop window canvas shapes mousePosRef = do
  (x, y) <- liftIO $ readIORef mousePosRef
  let myCamera = newCamera x y 10 (RGB 0 0 0)

  let distances = map (distance myCamera) myShapes
  let minDistance = minimum distances
  let myMarch = newMarch x y minDistance (RGB 10 221 169)

  draw canvas shapes
  drawShape canvas myMarch
  drawShape canvas myCamera
--(x, y) <- liftIO $ readIORef mousePosRef
-- let myCamera = newCamera x y 10 (RGB 0 0 0)
--drawShape canvas myCamera
