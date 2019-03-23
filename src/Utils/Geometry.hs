module Utils.Geometry where


distance :: (Float, Float) -> (Float, Float) -> Float
distance (a, b) (c, d) = sqrt $ (a - c) * (a - c) + (b - d) * (b - d)
