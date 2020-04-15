module GameOfLife
where

import qualified Graphics.Gloss as G

import Utils.ShapesColors(coloredRectangle, coloredSquare)

width = 500
height = 500
cellSize = 5

window :: G.Display
window = G.InWindow "Game of Life" (width, height) (10, 10)

background :: G.Color
background = G.black


data CellStatus = Alive | Dead deriving Eq

instance Show CellStatus where
    show Alive = "*"
    show Dead = " "


data Cell = Cell { x :: Int
                 , y :: Int
                 , status :: CellStatus
                 }
                 deriving Show

data GameState = GameState { cells :: [[Cell]]
                           , time :: Integer
                           }

instance Show GameState where
    show (GameState rows _) = unlines renderedRows
        where renderedRows = map renderRow rows
              renderRow = unwords . map (show . status)

gameStateFromBinaryRows :: [[Int]] -> GameState
gameStateFromBinaryRows binrows = GameState { time = 0
                                            , cells = mappedRows
                                            }
                                                where mappedRows = map (\(j, row) -> map (getCell j) (zip [0..] row)) (zip [0..] binrows)
                                                      getCell j (i, v) = Cell i j (if v == 1 then Alive else Dead)

rawRows :: [[Int]]
rawRows = [ [0, 0, 0, 0, 0]
          , [0, 0, 0, 0, 0]
          , [0, 0, 0, 0, 0]
          , [0, 0, 0, 0, 0]
          , [0, 0, 0, 0, 0]
          ]

type Element = [[CellStatus]]

elementGlider :: Element
elementGlider = [ [Dead, Alive, Dead]
                , [Dead, Dead, Alive]
                , [Alive, Alive, Alive]
                ]

insertElementAt :: Element -> (Int, Int) -> GameState -> GameState
insertElementAt elemRows (xx, yy) (GameState gCells t) = GameState { cells = prev ++ inserted ++ later, time = t}
    where prev = take yy gCells
          dropped = drop yy gCells
          toZip = take elemRowsCount dropped
          later = drop elemRowsCount dropped
          elemRowsCount = length elemRows
          inserted = zipWith replaceCells elemRows (zip toZip [0..])
          replaceCells elemRow (gameRow, i) = take xx gameRow ++ elemRowToCells i elemRow ++ drop (xx + length elemRow) gameRow
          elemRowToCells i row = map (\(j, stat) -> Cell { y = yy + j, x = xx + i, status = stat }) (zip [0..] row)

gState = insertElementAt elementGlider (2, 2) $ gameStateFromBinaryRows rawRows

getNthFromList :: Int -> [a] -> [a]
getNthFromList _ [] = []
getNthFromList n list
  | n < 0 = []
  | n >= length list = []
  | otherwise = [list !! n]

getNeighbors :: Cell -> GameState -> [Cell]
getNeighbors cell state = tail $ foldl (\a (i, j) -> a ++ getNthFromList j (concat $ getNthFromList i gCells)) [] indices
    where indices = [(cx+i, cy+j) | i <- [0, -1, 1], j <- [0, -1, 1]]
          cx = x cell
          cy = y cell
          gCells = cells state

renderGameState :: GameState -> G.Picture
renderGameState (GameState gCells _) = G.Pictures $
    zipWith (curry renderRow) [0..] gCells
        where renderRow (j, row) = G.Pictures $ map (renderCell j) (zip [0..] row)
              renderCell j (i, cell) = if status cell == Alive
                                  then G.translate (cellSize * i) (-cellSize * j) $ coloredRectangle G.green (cellSize-1) (cellSize-1)
                                  else G.Blank


drawing :: G.Picture
drawing = renderGameState gState
    {- 
drawing = G.Pictures [ coloredSquare G.green 10, G.translate 50 0 $coloredSquare G.blue 10
                     , G.translate (-50) 0 $ coloredSquare G.red 10
                     , G.translate 0 50 $coloredSquare G.yellow 10
                     , G.translate 0 (-50) $coloredSquare G.white 10
                     ]
                     -}

run  :: IO ()
run = do
    print gState
    G.display window background drawing
