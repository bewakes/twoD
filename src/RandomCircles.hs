module RandomCircles (
      run
) where

import System.Random
import Control.Monad (replicateM)

import Utils.Geometry

import qualified Graphics.Gloss as G


type Circle = (Float, Float, Float)

width = 700
height = 1000
maxRad = 100
minRad = 10

maxX = width / 2
minX = -maxX

maxY = height / 2
minY = -maxY

count = 100

widthHeight = (round width, round height)

getBoundedValue :: Float -> Float -> Float -> Float
getBoundedValue min max value = min + value * (max - min)

nRandomFloats :: Int -> IO [Float]
nRandomFloats count = replicateM count (randomIO :: IO Float)

doCirclesIntersect :: Circle -> Circle -> Float -> Bool
doCirclesIntersect (x1, y1, r1) (x2, y2, r2) offset = distance (x1, y1) (x2, y2) <= r1 + r2 + offset

doesCircleIntercectWithAny :: Circle -> [Circle] -> Float -> Bool
doesCircleIntercectWithAny circle circles offset = foldl
    (\a x -> doCirclesIntersect circle x offset || a) False circles

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
                if doesCircleIntercectWithAny circle circles 0 then f state
                else f (pure (cnt - 1, circle: circles))

generateRandomCircle :: IO Circle
generateRandomCircle = do
    x <- getBoundedValue minX maxX <$> (randomIO :: IO Float)
    y <- getBoundedValue minY maxY <$> (randomIO :: IO Float)
    r <- getBoundedValue minRad maxRad <$> (randomIO :: IO Float)
    return (x, y, r)

getCircle :: Circle -> G.Picture
getCircle (x, y, r) = G.Color G.white $ G.Translate x y $ G.ThickCircle 0 r

getCircles :: Int -> IO G.Picture
getCircles count = G.Pictures . map getCircle <$> generateNonIntersectingCircles count
