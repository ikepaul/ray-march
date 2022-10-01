module Draw where

import Graphics.UI.Threepenny
import Shapes

canvasHeight :: Double
canvasHeight = 600

canvasWidth :: Double
canvasWidth = 1000

draw :: Element -> [Shape] -> UI ()
draw canvas shapes = do
  pure canvas # set fillStyle (solidColor (RGB 255 255 255))
  fillRect (0, 0) canvasWidth canvasHeight canvas
  drawShapes canvas shapes

drawShapes :: Element -> [Shape] -> UI ()
drawShapes canvas = mapM_ (drawShape canvas)

drawShape :: Element -> Shape -> UI ()
drawShape canvas (Circle x y r c) = do
  beginPath canvas
  pure canvas # set fillStyle (solidColor c)
  arc (x, y) r 0 (2 * pi) canvas
  fill canvas
drawShape canvas (Rectangle x y w h c) = do
  pure canvas # set fillStyle (solidColor c)
  fillRect (x, y) w h canvas
drawShape canvas (Line x1 y1 x2 y2 w c) = do
  pure canvas # set strokeStyle ("rgb(" ++ (show . red) c ++ "," ++ (show . green) c ++ "," ++ (show . blue) c ++ ")")
  pure canvas # set lineWidth (if w == 0 then 1 else w)
  beginPath canvas
  moveTo (x1, y1) canvas
  lineTo (x2, y2) canvas
  stroke canvas

drawMarch :: Element -> Shape -> UI ()
drawMarch canvas (Circle x y r c) = do
  beginPath canvas
  pure canvas # set lineWidth 1
  pure canvas # set strokeStyle ("rgb(" ++ (show . red) c ++ "," ++ (show . green) c ++ "," ++ (show . blue) c ++ ")")
  arc (x, y) r 0 (2 * pi) canvas
  stroke canvas

drawMarches :: Element -> [Shape] -> UI()
drawMarches canvas = mapM_ (drawMarch canvas)