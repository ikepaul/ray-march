module Shapes where

import Graphics.UI.Threepenny (Color (RGB), Point)

data Shape
  = Rectangle {shapeX :: Double, shapeY :: Double, shapeWidth :: Double, shapeHeight :: Double, shapeColor :: Color}
  | Circle {shapeX :: Double, shapeY :: Double, radius :: Double, shapeColor :: Color}
  | Line {xStart :: Double, yStart :: Double, xEnd :: Double, yEnd :: Double, shapeWidth :: Double, shapeColor :: Color}
  deriving (Eq, Show)

type Camera = Shape

type March = Shape

newCamera :: Double -> Double -> Double -> Color -> Camera
newCamera = Circle

newMarch :: Double -> Double -> Double -> Color -> March
newMarch = Circle

myRectangle :: Shape
myRectangle = Rectangle 200 200 30 100 (RGB 0 100 100)

myCircle :: Shape
myCircle = Circle 33 33 99 (RGB 188 99 188)

myLine :: Shape
myLine = Line 400 400 500 500 30 (RGB 100 100 100)

myLine2 :: Shape
myLine2 = Line 400 100 500 200 0 (RGB 100 100 100)

myShapes :: [Shape]
myShapes = [myRectangle, myCircle, myLine, myLine2]

distance :: (Double, Double) -> Shape -> Double
distance (x1, y1) (Circle x2 y2 r _) = abs ((- r) + sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)))
distance (x1, y1) (Rectangle x2 y2 w h _) = minimum sides
  where
    sides =
      [ distance (x1, y1) (Line x2 y2 (x2 + w) y2 0 undefined),
        distance (x1, y1) (Line x2 y2 x2 (y2 + h) 0 undefined),
        distance (x1, y1) (Line x2 (y2 + h) (x2 + w) (y2 + h) 0 undefined),
        distance (x1, y1) (Line (x2 + w) y2 (x2 + w) (y2 + h) 0 undefined)
      ]

-- This implementation is right but when the line is of width zero and rotated things go bad
-- Fix if you want rotated rectangles or lines
distance (x1, y1) (Line x2 y2 x3 y3 0 _) = sqrt ((x1 - x4) * (x1 - x4) + (y1 - y4) * (y1 - y4))
  where
    x4
      | x2 - x3 == 0 = x2
      | (closestX > x2 && x2 > x3) || (closestX < x2 && x2 < x3) = x2
      | (closestX > x3 && x3 > x2) || (closestX < x3 && x3 < x2) = x3
      | otherwise = closestX
    y4
      | y2 - y3 == 0 = y2
      | (closestY > y2 && y2 > y3) || (closestY < y2 && y2 < y3) = y2
      | (closestY > y3 && y3 > y2) || (closestY < y3 && y3 < y2) = y3
      | otherwise = closestY
    closestX = (b * (b * x1 - a * y1) - a * c) / (a ^ 2 + b ^ 2)
    closestY = (a * (- b * x1 + a * y1) - b * c) / (a ^ 2 + b ^ 2)
    a = y2 - y3
    b = x3 - x2
    c = x2 * y3 - x3 * y2
distance (x1, y1) (Line x2 y2 x3 y3 w _) = minimum sides
  where
    sides =
      [ distance (x1, y1) (Line (x2 + (w / 2) * cos angle) (y2 + (w / 2) * sin angle) (x3 + (w / 2) * cos angle) (y3 + (w / 2) * sin angle) 0 undefined),
        distance (x1, y1) (Line (x2 - (w / 2) * cos angle) (y2 - (w / 2) * sin angle) (x3 - (w / 2) * cos angle) (y3 - (w / 2) * sin angle) 0 undefined),
        distance (x1, y1) (Line (x2 + (w / 2) * cos angle) (y2 + (w / 2) * sin angle) (x2 - (w / 2) * cos angle) (y2 - (w / 2) * sin angle) 0 undefined),
        distance (x1, y1) (Line (x3 + (w / 2) * cos angle) (y3 + (w / 2) * sin angle) (x3 - (w / 2) * cos angle) (y3 - (w / 2) * sin angle) 0 undefined)
      ]
    angle = if (x3 - x2) /= 0 then atan ((y3 - y2) / (x3 - x2)) + pi * 0.5 else pi

getIntersection :: Point -> Point -> Point -> Point -> Point
getIntersection (x1, y1) (x2, y2) (x3, y3) (x4, y4) = (x5, y5)
  where
    x5 = lerp x1 x2 t
    y5 = lerp y1 y2 t
    t = tTop / bottom
    u = uTop / bottom
    tTop = (x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)
    uTop = (y3 - y1) * (x1 - x2) - (x3 - x1) * (y1 - y2)
    bottom = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)

lerp :: Double -> Double -> Double -> Double
lerp a b t = a + (b - a) * t