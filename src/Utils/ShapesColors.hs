module Utils.ShapesColors
where

import qualified Graphics.Gloss as G


coloredRectangle :: G.Color -> Float -> Float -> G.Picture
coloredRectangle c a b = G.Color c $ G.rectangleSolid a b

coloredSquare :: G.Color -> Float -> G.Picture
coloredSquare c l = G.Color c $ G.rectangleSolid l l
