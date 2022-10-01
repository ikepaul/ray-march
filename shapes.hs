module Shapes where

import Graphics.UI.Threepenny (Color (RGB))

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

distance :: Camera -> Shape -> Double
distance (Circle x1 y1 _ _) (Circle x2 y2 r _) = (- r) + sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))
distance (Circle x1 y1 _ _) (Rectangle x2 y2 w h _) = minimum sides
  where
    sides =
      [ distance (Circle x1 y1 undefined undefined) (Line x2 y2 (x2 + w) y2 0 undefined),
        distance (Circle x1 y1 undefined undefined) (Line x2 y2 x2 (y2 + h) 0 undefined),
        distance (Circle x1 y1 undefined undefined) (Line x2 (y2 + h) (x2 + w) (y2 + h) 0 undefined),
        distance (Circle x1 y1 undefined undefined) (Line (x2 + w) y2 (x2 + w) (y2 + h) 0 undefined)
      ]

-- This implementation is right but when the line is of width zero and rotated things go bad
-- Fix if you want rotated rectangles or lines
distance (Circle x1 y1 _ _) (Line x2 y2 x3 y3 0 _) = sqrt ((x1 - x4) * (x1 - x4) + (y1 - y4) * (y1 - y4))
  where
    x4
      | abs (x1 - x2) < abs (x2 - x3) && abs (x1 - x3) < abs (x2 - x3) = x1
      | abs (x1 - x2) < abs (x1 - x3) = x2
      | otherwise = x3
    y4
      | abs (y1 - y2) < abs (y2 - y3) && abs (y1 - y3) < abs (y2 - y3) = y1
      | abs (y1 - y2) < abs (y1 - y3) = y2
      | otherwise = y3
distance (Circle x1 y1 _ _) (Line x2 y2 x3 y3 w _) = minimum sides
  where
    sides =
      [ distance (Circle x1 y1 undefined undefined) (Line (x2 + (w / 2) * cos angle) (y2 + (w / 2) * sin angle) (x3 + (w / 2) * cos angle) (y3 + (w / 2) * sin angle) 0 undefined),
        distance (Circle x1 y1 undefined undefined) (Line (x2 - (w / 2) * cos angle) (y2 - (w / 2) * sin angle) (x3 - (w / 2) * cos angle) (y3 - (w / 2) * sin angle) 0 undefined),
        distance (Circle x1 y1 undefined undefined) (Line (x2 + (w / 2) * cos angle) (y2 + (w / 2) * sin angle) (x2 - (w / 2) * cos angle) (y2 - (w / 2) * sin angle) 0 undefined),
        distance (Circle x1 y1 undefined undefined) (Line (x3 + (w / 2) * cos angle) (y3 + (w / 2) * sin angle) (x3 - (w / 2) * cos angle) (y3 - (w / 2) * sin angle) 0 undefined)
      ]
    angle = atan ((y3 - y2) / (x3 - x2)) + pi * 0.5
distance _ _ = error "Circle not provided as camera"

{-
slope*x = y
y/slope = x
w = abs . sqrt $ (Xa - Xb) ^ 2 + (Ya - Yb)^2
w = abs . sqrt $ (X1 - Xb) ^ 2 + (Ya - Yb)^2

x2

-}