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
  pure canvas # set lineWidth w
  beginPath canvas
  moveTo (x1, y1) canvas
  lineTo (x2, y2) canvas
  stroke canvas

drawWeird :: Element -> Shape -> Shape -> UI ()
drawWeird canvas (Circle x1 y1 _ _) (Line x2 y2 x3 y3 w _) = do
  pure canvas # set lineWidth 4
  drawShape canvas (Line (x2 + (w / 2) * cos angle) (y2 + (w / 2) * sin angle) (x3 + (w / 2) * cos angle) (y3 + (w / 2) * sin angle) 0 (RGB 255 255 0))
  where
    angle = atan ((y3 - y2) / (x3 - x2)) + pi * 0.5
