module RandomCircles (
      run
) where

import System.Random
import Control.Monad (replicateM)

import Utils.Geometry

import qualified Graphics.Gloss as G


type Circle = (Float, Float, Float)

width = 500
height = 700
maxRad = 80
minRad = 10

maxX = width / 2
minX = -maxX

maxY = height / 2
minY = -maxY

count = 100
circlesOffset = 0

widthHeight = (round width, round height)

getBoundedValue :: Float -> Float -> Float -> Float
getBoundedValue min max value = min + value * (max - min)

nRandomFloats :: Int -> IO [Float]
nRandomFloats count = replicateM count (randomIO :: IO Float)

doCirclesIntersect :: Circle -> Circle -> Float -> Bool
doCirclesIntersect (x1, y1, r1) (x2, y2, r2) offset = distance (x1, y1) (x2, y2) < r1 + r2 + offset

doesCircleIntercectWithAny :: Circle -> [Circle] -> Float -> Bool
doesCircleIntercectWithAny circle circles offset = foldl
    (\a x -> doCirclesIntersect circle x offset || a) False circles

getMaxNonIntersectingCircle :: Circle -> [Circle] -> Float -> Circle
getMaxNonIntersectingCircle circle circles offset = getMaxCircleLimit 0 circle circles offset
    where
        getMaxCircleLimit cnt (x, y, r) cs o
            | doesCircleIntercectWithAny (x, y, r+5) circles offset || cnt > 20 || r >= maxRad = (x, y, r)
            | otherwise = getMaxCircleLimit (cnt+1) (x, y, r+5) circles offset

run :: IO ()
run = do
    circles <- getCircles count
    G.display (G.InWindow "Random Circles" widthHeight (20,20)) G.black circles


generateNonIntersectingCircles :: Int -> IO [Circle]
generateNonIntersectingCircles count = do
    (_, circles) <- f (pure (count, []))
    return circles
    where
        f state = do
            (cnt, circles) <- state
            circle <- generateRandomCircle
            if cnt == 0 then return (cnt, circles)
            else
                if doesCircleIntercectWithAny circle circles circlesOffset then f state
                else let maxCircle = getMaxNonIntersectingCircle circle circles circlesOffset
                    in f (pure (cnt - 1, maxCircle : circles))

generateRandomCircle :: IO Circle
generateRandomCircle = do
    x <- getBoundedValue minX maxX <$> (randomIO :: IO Float)
    y <- getBoundedValue minY maxY <$> (randomIO :: IO Float)
    r <- getBoundedValue minRad maxRad <$> (randomIO :: IO Float)
    return (x, y, r)

getCircle :: Circle -> G.Picture
getCircle (x, y, r) = G.Color G.white $ G.Translate x y $ G.ThickCircle 0 (2*r)

getCircles :: Int -> IO G.Picture
getCircles count = G.Pictures . map getCircle <$> generateNonIntersectingCircles count
