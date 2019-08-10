module Physics.Kinematics
where

import qualified Utils.Geometry as UG

(***) :: UG.Point -> Float -> UG.Point
(***) (a, b) s = (a*s, b*s)

(+++) :: UG.Point -> UG.Point -> UG.Point
(+++) (a, b) (c, d) = (a + c , b + d)

collide :: (Float, UG.Point) -> (Float, UG.Point) -> (UG.Point, UG.Point)
collide (m1, u1) (m2, u2) = (v1, v2)
    where v1 = u1 *** (mdiff / msum) +++ u1 *** (2 * m2 / msum)
          v2 = u2 *** (-mdiff / msum) +++ u1 *** (2 * m1 / msum)
          mdiff = m1 - m2
          msum = m1 + m2
