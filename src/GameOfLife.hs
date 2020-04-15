module GameOfLife
where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Data.ViewPort

import Utils.ShapesColors(coloredRectangle, coloredSquare)

width = 200
height = 200
cellSize = 5 :: Int
fcellSize = fromIntegral cellSize :: Float

window :: G.Display
window = G.InWindow "Game of Life" (width, height) (10, 10)

background :: G.Color
background = G.black


data CellStatus = Alive | Dead deriving Eq

instance Show CellStatus where
    show Alive = "*"
    show Dead = "_"

toInt :: CellStatus -> Int
toInt Alive = 1
toInt Dead = 0

toBool :: CellStatus -> Bool
toBool Alive = True
toBool Dead = False

fromBool :: Bool -> CellStatus
fromBool True = Alive
fromBool False = Dead


data Cell = Cell { x :: Int
                 , y :: Int
                 , status :: CellStatus
                 }

instance Show Cell where
    show (Cell x y status) = show x ++ show y ++ " " ++ show status

data GameState = GameState { cells :: [[Cell]]
                           , time :: Integer
                           }

instance Show GameState where
    show (GameState rows _) = unlines renderedRows
        where renderedRows = map renderRow rows
              renderRow = unwords . map show

gameStateFromBinaryRows :: [[Int]] -> GameState
gameStateFromBinaryRows binrows = GameState { time = 0
                                            , cells = mappedRows
                                            }
                                                where mappedRows = map (\(j, row) -> map (getCell j) (zip [0..] row)) (zip [0..] binrows)
                                                      getCell j (i, v) = Cell i j (if v == 1 then Alive else Dead)

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
          elemRowToCells i row = map (\(j, stat) -> Cell { x = yy + j, y = xx + i, status = stat }) (zip [0..] row)


initialState :: Int -> Int -> Int -> GameState
initialState w h cellsize = GameState { cells = rows, time = 0 }
    where rows = map createRow [0..(rowsCount - 1)]
          createRow j = map (\i -> Cell { x = i, y = j, status = Dead }) [0..(colsCount-1)]
          rowsCount = round $ (fromIntegral h :: Float) / (fromIntegral cellsize :: Float)
          colsCount = round $ (fromIntegral w :: Float) / (fromIntegral cellsize :: Float)

gState = insertElementAt elementGlider (1, 1) $ initialState width height cellSize

getNthFromList :: Int -> [a] -> [a]
getNthFromList _ [] = []
getNthFromList n list = [list !! ind]
    where len = length list
          ind = (n + len) `mod` len

getNeighbors :: Cell -> GameState -> [Cell]
getNeighbors cell state = tail $ foldl (\a (j, i) -> a ++ getNthFromList j (concat $ getNthFromList i gCells)) [] indices
    where indices = [(cx+i, cy+j) | i <- [0, -1, 1], j <- [0, -1, 1]]
          cx = x cell
          cy = y cell
          gCells = cells state

getAliveNeighbors :: Cell -> GameState -> [Cell]
getAliveNeighbors c g = filter ((Alive ==) . status) $ getNeighbors c g

getAliveNeighborsCount :: Cell -> GameState -> Int
getAliveNeighborsCount c g = length $ getAliveNeighbors c g

renderGameState :: GameState -> G.Picture
renderGameState (GameState gCells _) = G.Pictures $ map renderCell (concat gCells)
    where renderCell cell = if status cell == Alive
                               then G.translate (fcellSize * fromIntegral (x cell) :: Float) (-fcellSize * fromIntegral (y cell) :: Float) $
                                   coloredRectangle G.green (fcellSize-1) (fcellSize-1)
                               else G.Blank


willCellSurvive :: Cell -> GameState -> Bool
willCellSurvive cell state
  | status cell == Alive = aliveCount == 2 || aliveCount == 3
  | otherwise = aliveCount == 3
  where aliveCount = getAliveNeighborsCount cell state

nextState :: GameState -> GameState
nextState g@(GameState rows t) = GameState { cells = newRows, time = t }
    where newRows = map nextRows (cells g)
          nextRows = map newDeadOrAlive
          newDeadOrAlive cell = Cell { x = x cell
                                     , y = y cell
                                     , status = fromBool $ willCellSurvive cell g
                                     }

drawing :: G.Picture
drawing = renderGameState gState

updateGame :: ViewPort -> Float -> GameState -> GameState
updateGame v _ = nextState

run  :: IO ()
run = G.simulate
    window
    G.black
    30 -- fps
    gState
    renderGameState
    updateGame


gcells = cells gState

_run = do
    print gState
    print $ getNeighbors ((gcells !! 1) !! 2) gState
    print $ map (\c -> (c, c `willCellSurvive` gState)) (concat $ cells gState)
    print $ nextState gState
