module Utils.Random

where

import System.Random
import Control.Monad

import qualified Utils.Conversion as UC

nRandomFloats :: Int -> IO [Float]
nRandomFloats count = replicateM count (randomIO :: IO Float)


randomChoice :: [a] -> IO a
randomChoice [] = return undefined
randomChoice list = do
    rand <- randomIO :: IO Float
    return $ list !! index rand
    where tmpindex rand = round (rand * UC.intToFloat (length list))
          index rand = min (tmpindex rand) (length list - 1)
