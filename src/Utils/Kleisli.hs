module Utils.Kleisli
where

import Control.Monad


kleisliPow :: (Monad m) => Int -> (a -> m a) -> a -> m a
kleisliPow 0 _ = return
kleisliPow n f = f >=> kleisliPow (n-1) f


cantorSet :: (Float, Float) -> [(Float, Float)]
cantorSet (a, b) = [(a, a + (b - a) / 3.0), (b - (b - a) / 3.0, b)]

-- main = print $ show $ kleisliPow 3 cantorSet (0, 1)
