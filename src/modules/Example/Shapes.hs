module Example.Shapes (
  Point
, Shape
, area
, nudge
, baseCircle
, baseRect
) where

data Point = Point Float Float
  deriving (Show)

data Shape
  = Circle Point Float
  | Rectangle Point Point
  deriving (Show)

-- |
-- 
-- >>> area $ baseCircle 100
-- 31415.928
-- >>> area $ baseRect 10 20
-- 200.0
area :: Shape -> Float
area (Circle (Point _ _) r) = pi * (r ^ (2 :: Integer))
area (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- |
-- 
-- >>> nudge (baseCircle 100) 3 4
-- Circle (Point 3.0 4.0) 100.0
-- >>> nudge (baseRect 10 20) 3 4
-- Rectangle (Point 3.0 4.0) (Point 13.0 24.0)
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) ax ay = Circle (Point (x + ax) (y + ay)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) ax ay =
    Rectangle (Point (x1 + ax) (y1 + ay)) (Point (x2 + ax) (y2 + ay))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect x y = Rectangle (Point 0 0) (Point x y)
