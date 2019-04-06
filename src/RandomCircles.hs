module RandomCircles (
      run
) where

import System.Random
import Control.Monad (replicateM)

import qualified Utils.Geometry as UG

import qualified Graphics.Gloss as G


width = 500
height = 700
maxRad = 80
minRad = 5 

maxX = width / 2
minX = -maxX

maxY = height / 2
minY = -maxY

count = 100
circlesOffset = 0

widthHeight = (round width, round height)

getBoundedValue :: Float -> Float -> Float -> Float
getBoundedValue min max value = min + value * (max - min)

getMaxNonIntersectingCircle :: UG.Circle -> [UG.Circle] -> Float -> UG.Circle
getMaxNonIntersectingCircle circle circles offset = getMaxCircleLimit 0 circle circles offset
    where
        getMaxCircleLimit cnt (x, y, r) cs o
            | UG.doesCircleIntercectWithAny (x, y, r+5) circles offset || cnt > 20 || r >= maxRad = (x, y, r)
            | otherwise = getMaxCircleLimit (cnt+1) (x, y, r+5) circles offset

generateNonIntersectingCircles :: Int -> IO [UG.Circle]
generateNonIntersectingCircles count = do
    (_, circles) <- f (pure (count, []))
    return circles
    where
        f state = do
            (cnt, circles) <- state
            circle <- generateRandomCircle
            if cnt == 0 then return (cnt, circles)
            else
                if UG.doesCircleIntercectWithAny circle circles circlesOffset then f state
                else let maxCircle = getMaxNonIntersectingCircle circle circles circlesOffset
                    in f (pure (cnt - 1, maxCircle : circles))

generateRandomCircle :: IO UG.Circle
generateRandomCircle = do
    x <- getBoundedValue minX maxX <$> (randomIO :: IO Float)
    y <- getBoundedValue minY maxY <$> (randomIO :: IO Float)
    r <- getBoundedValue minRad maxRad <$> (randomIO :: IO Float)
    return (x, y, r)

getCircle :: UG.Circle -> G.Picture
getCircle (x, y, r) = G.Color G.white $ G.Translate x y $ G.ThickCircle 0 (2*r)

getCircles :: Int -> IO G.Picture
getCircles count = G.Pictures . map getCircle <$> generateNonIntersectingCircles count

run :: IO ()
run = do
    circles <- getCircles count
    G.display (G.InWindow "Random Circles" widthHeight (20,20)) G.black circles
