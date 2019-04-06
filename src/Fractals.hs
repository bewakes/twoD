module Fractals (
      run
) where

import qualified Utils.Geometry as UG
import qualified Utils.Random as UR

import qualified Graphics.Gloss as G
import Utils.Kleisli

piBy3 = pi / 3.0
piBy4 = pi / 4.0
piBy2 = pi / 2.0
twoPi = pi * 2.0

scaleFactor = 1

type Color = G.Picture -> G.Picture

renderPoint :: (G.Picture -> G.Picture) -> UG.Point -> G.Picture
renderPoint color (a, b) = color $ G.translate a b $ G.rectangleSolid 1 1 

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
    where (_, a) = UG.fractionLine 0.5 (p1, p2)
          (_, b) = (UG.fractionLine scale . UG.rotateLineAbout a piBy3) (a, p1)
          (_, c) = (UG.fractionLine scale . UG.rotateLineAbout a (2*piBy3)) (a, p1)

iterateTriangles :: Int -> UG.Triangle -> [UG.Triangle]
iterateTriangles 0 triangle = []
iterateTriangles count triangle = triangle: concatMap nextIterate children
    where nextIterate = iterateTriangles (count - 1)
          children = getChildren triangle

triangle = UG.getEqTriangleAtOrigin 100

renderTriangles :: IO ()
renderTriangles = G.display (G.InWindow "Fractals" (500, 500) (20, 20)) G.black $ G.Pictures $
    map (renderTriangle (G.Color G.blue)) (iterateTriangles 6 triangle)
    -- map (renderTriangle (G.Color G.blue)) (kleisliPow 6 getChildren triangle)

renderPictures :: Int -> Int -> String -> [G.Picture] -> IO ()
renderPictures x y title = G.display (G.InWindow title (x, y) (20, 20)) G.black . G.Pictures 

renderPictures500x500 = renderPictures 500 500

triangleVerts :: [UG.Point]
triangleVerts = [(200 * cos (2*k*pi/3), 200 * sin (2*k*pi/3)) | k <- [0..2]]

hexVerts :: [UG.Point]
hexVerts = [(400 * cos (2*k*pi/6), 400 * sin (2*k*pi/6)) | k <- [0..5]]

renderSerspinskiRandom :: Int -> [UG.Point] -> UG.Point -> IO ()
renderSerspinskiRandom iteration trianglePoints = iterate iteration (return [] :: IO [UG.Point])
    where iterate n ioPics currPoint = do
                points <- foldl getPoints (pure [currPoint]) [1..n]
                renderPictures500x500 "serspinski" (map (renderPoint (G.Color G.white)) points)
          getPoints acc x = do
                currPoints <- acc
                trianglePoint <- UR.randomChoice trianglePoints
                return $ UG.midPoint (head currPoints) trianglePoint: currPoints

serspinskiTriangleFunc :: UG.Point -> [UG.Point]
serspinskiTriangleFunc point = [UG.scaleAbout 0.5 q point | q <- triangleVerts]

serspinskiHexFunc :: UG.Point -> [UG.Point]
serspinskiHexFunc point = [UG.scaleAbout 0.33333 q point | q <- hexVerts]

renderSerspinskiMonadic :: [UG.Point] -> IO ()
renderSerspinskiMonadic points 
    = renderPictures500x500 "Monadic Serspinski" (map (renderPoint (G.Color G.blue)) points)

-- run = renderSerspinskiRandom 9000 triangleVerts (0.0, 0.0)
-- run = renderSerspinskiMonadic $ kleisliPow 8 serspinskiTriangleFunc (0.0, 0.0)

run = renderSerspinskiMonadic $ kleisliPow 6 serspinskiHexFunc (0.0, 0.0)
