module Utils.GameObject
where

import qualified Graphics.Gloss as G

import qualified Utils.Geometry as UG

class (Eq a, Show a) => Object a where
    render :: a -> G.Picture
    -- collision :: 
