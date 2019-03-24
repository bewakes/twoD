module Utils.Geometry where

type Circle = (Float, Float, Float)

distance :: (Float, Float) -> (Float, Float) -> Float
distance (a, b) (c, d) = sqrt $ (a - c) * (a - c) + (b - d) * (b - d)

doCirclesIntersect :: Circle -> Circle -> Float -> Bool
doCirclesIntersect (x1, y1, r1) (x2, y2, r2) offset = distance (x1, y1) (x2, y2) < r1 + r2 + offset

doesCircleIntercectWithAny :: Circle -> [Circle] -> Float -> Bool
doesCircleIntercectWithAny circle circles offset = foldl
    (\a x -> doCirclesIntersect circle x offset || a) False circles

