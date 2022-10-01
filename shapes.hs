module Shapes where

data Shape
  = Rectangle {x :: Double, y :: Double, w :: Double, h :: Double}
  | Circle {x :: Double, y :: Double, r :: Double}
  | Line {xStart :: Double, yStart :: Double, xEnd :: Double, yEnd :: Double}
  deriving (Eq, Show)
