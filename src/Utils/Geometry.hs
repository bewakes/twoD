module Utils.Geometry where

type Circle = (Float, Float, Float)

type Point = (Float, Float)

type Line = (Point, Point)

data Triangle = Triangle Point Point Point
    deriving Show

distance :: (Float, Float) -> (Float, Float) -> Float
distance (a, b) (c, d) = sqrt $ (a - c) * (a - c) + (b - d) * (b - d)

add :: Point -> Point -> Point
add (a, b) (c, d) = (a+c, b+d)

mul :: Point -> Float -> Point
mul (a, b) s = (a*s, b*s)

normalize :: Point -> Point
normalize (x, y) = (x / d, y / d)
    where d = sqrt $ x*x + y*y

doCirclesIntersect :: Circle -> Circle -> Float -> Bool
doCirclesIntersect (x1, y1, r1) (x2, y2, r2) offset = distance (x1, y1) (x2, y2) < r1 + r2 + offset

doesCircleIntercectWithAny :: Circle -> [Circle] -> Float -> Bool
doesCircleIntercectWithAny circle circles offset = foldl
    (\a x -> doCirclesIntersect circle x offset || a) False circles

midPoint :: Point -> Point -> Point
midPoint (a, b) (c, d) = ( (a + c) / 2, (b + d) / 2)

fractionLine :: Float -> Line -> Line
fractionLine fraction (start@(a, b), end@(c, d)) = (
      (a, b)
    , (a + fraction * (c - a), b + fraction * (d - b))
  )

rotateLineAbout :: Point -> Float -> Line -> Line
rotateLineAbout rotatePoint angle (start, end)
    = (
          rotateAboutPoint rotatePoint angle start
        , rotateAboutPoint rotatePoint angle end
    )

rotateAboutPoint :: Point -> Float -> Point -> Point
rotateAboutPoint center@(cx, cy) alpha
    = translateBy center . rotateAboutOrigin alpha . translateBy (-cx, -cy)

scale :: Float -> Point -> Point
scale f (x, y) = (f * x, f * y)

scaleAbout :: Float -> Point -> Point -> Point
scaleAbout f q p = ((1 - f) `scale` q) `translateBy` (f `scale` p)

rotateAboutOrigin :: Float -> Point -> Point
rotateAboutOrigin alpha (x, y)
    = (
          x * cos alpha - y * sin alpha
        , x * sin alpha + y * cos alpha
    )

translateBy :: Point -> Point -> Point
translateBy (a, b) (x, y) = (a + x, b + y)

getEqTriangleAtOrigin :: Float -> Triangle
getEqTriangleAtOrigin radius = Triangle a b c
    where a = (0.0, radius)
          b = rotateAboutOrigin (2.0 * pi / 3.0) a
          c = rotateAboutOrigin (2.0 * pi / 3.0) b


circlesCollide :: Circle -> Circle -> Bool
circlesCollide (x1, y1, r1) (x2, y2, r2) = distance (x1, y1) (x2, y2) <= r1 + r2 

circleCollideWithLine :: Circle -> Line -> Bool
circleCollideWithLine (x, y, r) ((x1, y1), (x2, y2)) = not outsideLine && distFromLine <= r
    where outsideLine = ((x - x1)*(x2-x1) + (y-y1) * (y2-y1)) * ((x - x2)*(x1-x2) + (y-y2) * (y1-y2)) > 0
          distFromLine = abs ((y2-y1)*x - (x2-x1)*y + x2*y1 - x1*y2) / distance (x1, y1) (x2, y2)
