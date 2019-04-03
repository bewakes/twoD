module Fractals (
      run
) where

import qualified Utils.Geometry as UG

import qualified Graphics.Gloss as G

piBy3 = pi / 3.0
piBy4 = pi / 4.0
piBy2 = pi / 2.0
twoPi = pi * 2.0

scaleFactor = 1

type Color = G.Picture -> G.Picture

renderTriangle :: Color -> UG.Triangle -> G.Picture
renderTriangle color (UG.Triangle a b c) = color $ G.polygon [a, b, c]

getChildren :: UG.Triangle -> [UG.Triangle]
getChildren (UG.Triangle a b c) = [
      getTriangleForSide scaleFactor (a, b)
    , getTriangleForSide scaleFactor (b, c)
    , getTriangleForSide scaleFactor (c, a)
    ]


getTriangleForSide :: Float -> UG.Line -> UG.Triangle
getTriangleForSide scale (p1, p2) = UG.Triangle a b c
    where a = UG.midPoint p1 p2
          (_, b) = (UG.fractionLine scale . UG.rotateLineAbout a piBy3) (a, p1)
          (_, c) = (UG.fractionLine scale . UG.rotateLineAbout a (2*piBy3)) (a, p1)

triangle = UG.getEqTriangleAtOrigin 100

run :: IO ()
run = G.display (G.InWindow "Fractals" (500, 500) (20, 20)) G.black $ G.Pictures $
    map (renderTriangle (G.Color G.blue)) (getChildren triangle)
    ++ [renderTriangle (G.Color G.red) triangle]
