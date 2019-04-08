module Physics.Kinematics
where

import qualified Utils.Geometry as UG

mult :: UG.Point -> Float -> UG.Point
mult (a, b) s = (a*s, b*s)

add :: UG.Point -> UG.Point -> UG.Point
add (a, b) (c, d) = (a + c , b + d)

collide :: (Float, UG.Point) -> (Float, UG.Point) -> (UG.Point, UG.Point)
collide (m1, u1) (m2, u2) = (v1, v2)
    where v1 = u1 `mult` (mdiff / msum) `add` u1 `mult` (2 * m2 / msum)
          v2 = u2 `mult` (-mdiff / msum) `add` u1 `mult` (2 * m1 / msum)
          mdiff = m1 - m2
          msum = m1 + m2
