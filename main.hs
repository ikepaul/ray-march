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

  body <- getBody window
  pure body
    #+ [column [element canvas]]

  on mousedown canvas $ \(x, y) -> do
    let myCircle = Circle x y 30 (RGB 188 0 188)
    drawShape canvas myCircle

  mousePosRef <- liftIO $ newIORef ((0, 0) :: Point)
  angleRef <- liftIO $ newIORef (0 :: Double)

  on keydown body $ \keyCode -> do
    liftIO $ modifyIORef angleRef (+0.1)
    gameLoop window canvas mousePosRef angleRef


  on mousemove canvas $ \(x, y) -> do
    liftIO $ writeIORef mousePosRef (x, y)
    gameLoop window canvas mousePosRef angleRef

  body <- getBody window
  pure body
    #+ [column [element canvas]]

  timer1 <- set interval 3000 timer
  start timer1

  -- on tick timer1 $ \() -> gameLoop window canvas myShapes mousePosRef

  {- timer2 <- set interval 3000 timer
  start timer2
  on tick timer2 $ \() -> do
    return () -}
  --let myRectangle = Rectangle 200 200 30 100 (RGB 0 100 100)
  --drawShape canvas myRectangle

  --gameLoop window canvas

  return ()

gameLoop :: Window -> Element -> IORef Point -> IORef Double -> UI ()
gameLoop window canvas mousePosRef angleRef = do
  (x, y) <- liftIO $ readIORef mousePosRef
  angle <- liftIO $ readIORef angleRef
  let myCamera = newCamera x y 10 (RGB 0 0 0)
  let myMarches = startMarch (x, y) angle

  draw canvas theShapes
  drawShape canvas myCamera
  drawMarches canvas myMarches

startMarch :: (Double, Double) -> Double -> [March]
startMarch (x, y) angle = newMarch x y (minDistance ) (RGB 10 221 169) : nextMarch
  where
    nextMarch 
      | minDistance < 5 || x > canvasWidth || y > canvasHeight || x < 0 || y < 0= []
      | otherwise =startMarch ( x + minDistance * cos angle, y + minDistance * sin angle) angle
    minDistance = minimum distances
    distances = map (distance (x, y)) theShapes

wall1 = Line 0 0 canvasWidth 0 0 (RGB 0 0 0)
wall2 = Line 0 0 0 canvasHeight 0 (RGB 0 0 0)
wall3 = Line canvasWidth 0 canvasWidth canvasHeight 0 (RGB 0 0 0)
wall4 = Line 0 canvasHeight canvasWidth canvasHeight 0 (RGB 0 0 0)

theShapes = wall1:wall2:wall3:wall4 : myShapes
