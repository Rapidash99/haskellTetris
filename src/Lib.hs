module Lib
    ( tetrisActivity
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss (play)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact
import System.Random (randoms, mkStdGen)

-- | Coordinates:
--
-- (x, y)
type Coords = (Int, Int)

-- | Field size:
--
-- (x, y)
type Size = Coords

-- | Cell of which fields are composed
type Cell = (Coords, Color)

-- | Player score
type Score = Int

-- | Game speed. Not implemented in MVP
--type Speed = Double

-- | Tetrimino direction
data Direction = UpDir | LeftDir | RightDir | DownDir

-- | Tetrimino type
data TetriminoType = J | I | O | L | Z | T | S

-- | Tetrimino
data Tetrimino = Tetrimino
  { type'        :: TetriminoType
  , coords       :: [Coords]
  }

-- | Field
data Field = Field
  { size             :: Size
  , cells            :: [[Cell]]
  , currentTetrimino :: Tetrimino
  , rand             :: [Int]
  , score            :: Score
  , nextTetrimino    :: Tetrimino
  }

-- | World
data World = World
  { field :: Field
  --, speed :: Speed
  }

---------------------------------------------------------------
-- | Draw functions:
---------------------------------------------------------------

drawScore :: Score -> Picture
drawScore score = translate 0 150 (text ("Score: " ++ show score))

drawCell :: Cell -> Picture
drawCell ((x, y), color') = translate x' y' (color color' (rectangleSolid 0.95 0.95))
  where
    x' = fromIntegral x
    y' = fromIntegral (-y)

drawCells :: [Cell] -> Picture
drawCells cells = pictures (map drawCell cells)

drawTetrimino :: Tetrimino -> Picture
drawTetrimino tetrimino = pictures (map drawCell (tetriminoToCells tetrimino))

drawNextTetrimino :: Tetrimino -> Picture
drawNextTetrimino tetrimino
   = translate 11 2 (scale 0.015 0.015 (text ("Next: ")))
  <> translate 12 4 (drawTetrimino tetrimino)

drawField :: Field -> Picture
drawField field
   = drawCells (concat cells)
  <> drawTetrimino currentTetrimino
  <> (scale 0.007 0.007 (drawScore score))
  <> scale 0.5 0.5 (drawNextTetrimino nextTetrimino)
  where
    Field _ cells currentTetrimino _ score nextTetrimino = field

drawWorld :: World -> Picture
drawWorld (World field) = translate (-100) 180 (scale 20 20 (drawField field))

---------------------------------------------------------------
-- | Translation functions:
---------------------------------------------------------------

colorToType :: Color -> TetriminoType
colorToType color'
  | color' == blue   = J
  | color' == cyan   = I
  | color' == yellow = O
  | color' == orange = L
  | color' == red    = Z
  | color' == violet = T
  | color' == green  = S
  | otherwise = O

typeToColor :: TetriminoType -> Color
typeToColor J = blue
typeToColor I = cyan
typeToColor O = yellow
typeToColor L = orange
typeToColor Z = red
typeToColor T = violet
typeToColor S = green

dirToCoords :: Direction -> Coords
dirToCoords dir = case dir of
  LeftDir  -> (-1, 0)
  RightDir -> (1, 0)
  DownDir  -> (0, 1)
  UpDir    -> (0, -1)

coordsToDir :: Coords -> Maybe Direction
coordsToDir coords = case coords of
  (-1, 0) -> Just LeftDir
  (1, 0)  -> Just RightDir
  (0, 1)  -> Just DownDir
  (0, -1) -> Just UpDir
  _       -> Nothing

tetriminoToCells :: Tetrimino -> [Cell]
tetriminoToCells (Tetrimino type' coords) = map (\coordinate -> (coordinate, typeToColor type')) coords

---------------------------------------------------------------
-- | Update functions:
---------------------------------------------------------------

-- | moves current tetrimino if can
-- if cannot move down, updates the field
tryMove :: Direction -> Field -> Field
tryMove direction field = case can of
  True  -> move direction field
  False -> newField
  where
    can = canMove direction field
    newField = case direction of
      DownDir -> layTetrimino field
      _       -> field

-- | checks if you can move current tetrimino
canMove :: Direction -> Field -> Bool
canMove direction field = can
  where
    Field size cells tetrimino _ _ _ = field
    Tetrimino type' coords           = tetrimino

    can             = notOutOfBorders && notIntersects
    notOutOfBorders = not (areOutOfBorders newCoords size)
    notIntersects   = not (doesIntersects (Tetrimino type' newCoords) (concat cells))
    newCoords       = map (\(x, y) -> (x + plusX, y + plusY)) coords
    (plusX, plusY)  = dirToCoords direction

-- | moves current tetrimino (only to use in function tryMove)
move :: Direction -> Field -> Field
move direction field = newField
  where
    Field size cells tetrimino rand score nextTetrimino = field
    Tetrimino type' coords                              = tetrimino

    newField       = Field size cells newTetrimino rand score nextTetrimino
    newTetrimino   = Tetrimino type' newCoords
    newCoords      = map (\(x, y) -> (x + plusX, y + plusY)) coords
    (plusX, plusY) = dirToCoords direction

-- | drops current tetrimino down
dropTetrimino :: Field -> Field
dropTetrimino field = newField -- to implement
  where
    Field size cells tetrimino rand score nextTetrimino = field
    Tetrimino type' coords                              = tetrimino
    
    newField = recDrop (Field size cells tetrimino rand (score + 20) nextTetrimino)

recDrop :: Field -> Field
recDrop field = newField
  where
    can      = canMove DownDir field
    newField = case can of 
      True  -> recDrop (move DownDir field)
      False -> field

-- | rotates current tetrimino by 90° left if can
tryRotateLeft :: Field -> Field
tryRotateLeft field = case can of
  True  -> rotateLeft field
  False -> field
  where
    can = canRotateLeft field

-- | rotates current tetrimino by 90° right if can
tryRotateRight :: Field -> Field
tryRotateRight field = case can of
  True  -> rotateRight field
  False -> field
  where
    can = canRotateRight field

-- | checks if you can rotate current tetrimino by 90° left
canRotateLeft :: Field -> Bool
canRotateLeft field = can
  where
    Field size cells tetrimino _ _ _     = field

    can                                  = notOutOfBorders && notIntersects
    notOutOfBorders                      = not (areOutOfBorders newCoords size)
    notIntersects                        = not (doesIntersects newTetrimino (concat cells))
    newTetrimino@(Tetrimino _ newCoords) = rotateTetriminoLeft tetrimino

-- | checks if you can rotate current tetrimino by 90° right
canRotateRight :: Field -> Bool
canRotateRight field = can
  where
    Field size cells tetrimino _ _ _     = field

    can                                  = notOutOfBorders && notIntersects
    notOutOfBorders                      = not (areOutOfBorders newCoords size)
    notIntersects                        = not (doesIntersects newTetrimino (concat cells))
    newTetrimino@(Tetrimino _ newCoords) = rotateTetriminoRight tetrimino

-- | rotates current tetrimino by 90° left (only to use in function tryRotateLeft)
rotateLeft :: Field -> Field
rotateLeft field = Field size cells newTetrimino rand score nextTetrimino
  where
    Field size cells tetrimino rand score nextTetrimino = field
    
    newTetrimino = rotateTetriminoLeft tetrimino

-- | rotates current tetrimino by 90° right (only to use in function tryRotateRight)
rotateRight :: Field -> Field
rotateRight field = Field size cells newTetrimino rand score nextTetrimino
  where
    Field size cells tetrimino rand score nextTetrimino = field
    
    newTetrimino = rotateTetriminoRight tetrimino

-- | rotates given tetrimino by 90° left
rotateTetriminoLeft :: Tetrimino -> Tetrimino
rotateTetriminoLeft (Tetrimino type' coords) = Tetrimino type' newCoords
  where
    (x1, y1): (x2, y2): _ = coords
    newCoords = case (type') of
      O -> coords

      J | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1, y1 - 2)] ++ [(x1 + 1, y1 - 2)]
      J | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1, y1 + 2)] ++ [(x1 - 1, y1 + 2)]
      J | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 2, y1)] ++ [(x1 + 2, y1 + 1)]
      J | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 2, y1)] ++ [(x1 - 2, y1 - 1)]

      I | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1, y1 - 2)] ++ [(x1, y1 - 3)]
      I | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1, y1 + 2)] ++ [(x1, y1 + 3)]
      I | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 2, y1)] ++ [(x1 + 3, y1)]
      I | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 2, y1)] ++ [(x1 - 3, y1)]

      L | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1, y1 - 2)] ++ [(x1 - 1, y1 - 2)]
      L | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1, y1 + 2)] ++ [(x1 + 1, y1 + 2)]
      L | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 2, y1)] ++ [(x1 + 2, y1 - 1)]
      L | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 2, y1)] ++ [(x1 - 2, y1 + 1)]

      Z | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1 + 1, y1 - 1)] ++ [(x1 + 1, y1 - 2)]
      Z | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1 - 1, y1 + 1)] ++ [(x1 - 1, y1 + 2)]
      Z | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 1, y1 + 1)] ++ [(x1 + 2, y1 + 1)]
      Z | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 - 2, y1 - 1)]

      T | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 + 1, y1 - 1)]
      T | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1 - 1, y1 + 1)] ++ [(x1 + 1, y1 + 1)]
      T | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 1, y1 - 1)] ++ [(x1 + 1, y1 + 1)]
      T | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 - 1, y1 + 1)]

      S | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1 + 1, y1 + 1)] ++ [(x1 + 1, y1 + 2)]
      S | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 - 1, y1 - 2)]
      S | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 1, y1 + 1)] ++ [(x1 - 2, y1 + 1)]
      S | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 1, y1 - 1)] ++ [(x1 + 2, y1 - 1)]

      _ -> coords
      
-- | rotates given tetrimino by 90° right
rotateTetriminoRight :: Tetrimino -> Tetrimino
rotateTetriminoRight (Tetrimino type' coords) = Tetrimino type' newCoords
  where
    (x1, y1): (x2, y2): _ = coords
    newCoords = case (type') of
      O -> coords

      J | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1, y1 + 2)] ++ [(x1 - 1, y1 + 2)]
      J | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1, y1 - 2)] ++ [(x1 + 1, y1 - 2)]
      J | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 2, y1)] ++ [(x1 - 2, y1 - 1)]
      J | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 2, y1)] ++ [(x1 + 2, y1 + 1)]

      I | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1, y1 + 2)] ++ [(x1, y1 + 3)]
      I | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1, y1 - 2)] ++ [(x1, y1 - 3)]
      I | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 2, y1)] ++ [(x1 - 3, y1)]
      I | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 2, y1)] ++ [(x1 + 3, y1)]

      L | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1, y1 + 2)] ++ [(x1 + 1, y1 + 2)]
      L | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1, y1 - 2)] ++ [(x1 - 1, y1 - 2)]
      L | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 2, y1)] ++ [(x1 - 2, y1 + 1)]
      L | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 2, y1)] ++ [(x1 + 2, y1 - 1)]

      Z | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1 - 1, y1 + 1)] ++ [(x1 - 1, y1 + 2)]
      Z | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1 + 1, y1 - 1)] ++ [(x1 + 1, y1 - 2)]
      Z | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 - 2, y1 - 1)]
      Z | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 1, y1 + 1)] ++ [(x1 + 2, y1 + 1)]

      T | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1 - 1, y1 + 1)] ++ [(x1 + 1, y1 + 1)]
      T | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 + 1, y1 - 1)]
      T | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 - 1, y1 + 1)]
      T | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 1, y1 - 1)] ++ [(x1 + 1, y1 + 1)]

      S | x2 == x1 + 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 - 1)] ++ [(x1 - 1, y1 - 1)] ++ [(x1 - 1, y1 - 2)]
      S | x2 == x1 - 1 && y2 == y1 -> [(x1, y1)] ++ [(x1, y1 + 1)] ++ [(x1 + 1, y1 + 1)] ++ [(x1 + 1, y1 + 2)]
      S | x2 == x1 && y2 == y1 + 1 -> [(x1, y1)] ++ [(x1 + 1, y1)] ++ [(x1 + 1, y1 - 1)] ++ [(x1 + 2, y1 - 1)]
      S | x2 == x1 && y2 == y1 - 1 -> [(x1, y1)] ++ [(x1 - 1, y1)] ++ [(x1 - 1, y1 + 1)] ++ [(x1 - 2, y1 + 1)]

      _ -> coords

-- | returns the field without completed rows
eliminateRows :: Field -> Field
eliminateRows field = recEliminateRows 0 field

-- | recursively tries to eliminate every row
recEliminateRows :: Int -> Field -> Field
recEliminateRows rowNumber field = case (rowNumber < length cells) of
  True  -> recEliminateRows (rowNumber + 1) newCells
  False -> field
  where
    Field size cells currentTetrimino rand score nextTetrimino = field
    
    newCells = tryEliminateRow (cells !! rowNumber) field

-- | tries to remove a certain row in a field
tryEliminateRow :: [Cell] -> Field -> Field
tryEliminateRow cells field = case can of
  True  -> newField
  False -> field
  where
    Field size rows currentTetrimino rand score nextTetrimino = field
    
    can      = canEliminateRow cells
    newRows  = eliminateRow cells rows
    newField = Field size newRows currentTetrimino rand (score + 100) nextTetrimino

-- | checks if you can remove a certain row in a field
canEliminateRow :: [Cell] -> Bool
canEliminateRow cells = isRowFull cells

-- | increases coordinate y by 1 in a list of Cells
incrementY :: [Cell] -> [Cell]
incrementY [] = []
incrementY (((x, y), color): cells) = ((x, y + 1), color): incrementY cells

-- | removes a certain row in a field (only to use in function tryEliminateRow)
eliminateRow :: [Cell] -> [[Cell]] -> [[Cell]]
eliminateRow cells rows = highPart ++ lowPart
  where
    (rows1, rows2) = break ( == cells) rows
    lowPart        = drop 1 rows2
    highPart       = newRow: (map incrementY rows1)
    newRow         = [((0, 0), white),((1, 0), white),((2, 0), white),((3, 0), white),((4, 0), white),
                      ((5, 0), white),((6, 0), white),((7, 0), white),((8, 0), white),((9, 0), white)]

-- | updates the field when tetrimino lays down
layTetrimino :: Field -> Field
layTetrimino field = newField
  where
    Field size cells currentTetrimino rand score nextTetrimino = field
    
    fieldWithTetrimino = Field size newCells nextTetrimino newRand score (getRandomTetrimino randInt)
    newCells           = mergeAllWithTetrimino cells currentTetrimino
    eliminatedField    = eliminateRows fieldWithTetrimino
    newField           = case (isGameOver eliminatedField) of
      True  -> initialField (10, 20) rand
      False -> eliminatedField
    (first, newRand)   = splitAt 1 rand
    randInt            = head first

-- | changes colors of cells at the place of tetrimino
mergeAllWithTetrimino :: [[Cell]] -> Tetrimino -> [[Cell]]
mergeAllWithTetrimino [] _ = []
mergeAllWithTetrimino (row: rows) tetrimino = [newRow] ++ mergeAllWithTetrimino rows tetrimino
  where
    newRow = map (\cell -> mergeCell cell tetrimino) row

-- | changes color of cell if there is a tetrimino
mergeCell :: Cell -> Tetrimino -> Cell
mergeCell cell@(coordinate, color) (Tetrimino type' coords) = case (elem coordinate coords) of
  True  -> (coordinate, typeToColor type')
  False -> cell

---------------------------------------------------------------
-- | Helper functions:
---------------------------------------------------------------

-- | generates random tetrimino
getRandomTetrimino :: Int -> Tetrimino
getRandomTetrimino rand = case (rand `mod` 7) of
  0 -> Tetrimino O [(5, 0), (6, 0), (6, 1), (5, 1)]
  1 -> Tetrimino J [(5, 0), (5, 1), (5, 2), (4, 2)]
  2 -> Tetrimino I [(5, 0), (6, 0), (7, 0), (8, 0)]
  3 -> Tetrimino L [(5, 0), (5, 1), (5, 2), (6, 2)]
  4 -> Tetrimino Z [(5, 0), (6, 0), (6, 1), (7, 1)]
  5 -> Tetrimino T [(5, 0), (5, 1), (4, 1), (6, 1)]
  6 -> Tetrimino S [(5, 0), (5, 1), (6, 1), (6, 2)]

-- | checks if game is over by checking if you can place a new tetrimino
isGameOver :: Field -> Bool
isGameOver (Field _ cells tetrimino _ _ _) = is
  where
    is = doesIntersects tetrimino (concat cells)

-- | checks if tetrimino intersects with cells
doesIntersects :: Tetrimino -> [Cell] -> Bool
doesIntersects _ [] = False
doesIntersects (Tetrimino _ []) _ = False
doesIntersects (Tetrimino type' (c: cs)) cells = (elem c nonWhiteCoords) || (doesIntersects (Tetrimino type' cs) cells)
  where
    nonWhite = filter (\(_, color') -> color' /= white) cells -- O(n^2), need to fix
    nonWhiteCoords = map fst nonWhite

-- | checks if coordinate is out of borders
isOutOfBorders :: Coords -> Size -> Bool
isOutOfBorders (x, y) (sizeX, sizeY) = x < 0 || x >= sizeX || y < 0 || y >= sizeY

-- | checks if coordinates are out of borders
areOutOfBorders :: [Coords] -> Size -> Bool
areOutOfBorders coords size = any (\coordinate -> isOutOfBorders coordinate size) coords

-- | checks if given cell is occupied
isCellOccupied :: Cell -> Bool
isCellOccupied (_, color') = color' /= white

-- | checks if in a given row all cells are occupied
isRowFull :: [Cell] -> Bool
isRowFull cells = all isCellOccupied cells

-- | checks if given row has no occupied cells
isRowFree :: [Cell] -> Bool
isRowFree cells = all (not . isCellOccupied) cells

---------------------------------------------------------------
-- | Initial objects generators:
---------------------------------------------------------------

-- | takes sizes of field and returns its cells (for now sizes are not used)
initialCells :: (Int, Int) -> [[Cell]]
initialCells (x, y) = cells
  where
    xs     = take x (iterate (+1) 0)
    ys     = take y (iterate (+1) 0)
    colors = replicate x white
    cells  = map (\row -> zip (zip xs (replicate x row)) colors) ys
    

-- | takes a sizes of new field and an infinite list of Ints and returns new World
initialField :: (Int, Int) -> [Int] -> Field
initialField size rand = Field
  { size             = size
  , cells            = initialCells size
  , currentTetrimino = getRandomTetrimino (first2 !! 0)
  , rand             = rest
  , score            = 0
  , nextTetrimino    = getRandomTetrimino (first2 !! 1)
  }
  where
    (first2, rest) = splitAt 2 rand

-- | takes an infinite list of Ints and returns new World
initialWorld :: [Int] -> World
initialWorld rand = World (initialField (10, 20) rand)

---------------------------------------------------------------
-- | Game handling functions
---------------------------------------------------------------

updateWorld :: Float -> World -> World
updateWorld _dt (World field) = World (newField)
  where
    (Field size cells currentTetrimino rand score nextTetrimino) = tryMove DownDir field
    newField = Field size cells currentTetrimino rand (score + 1) nextTetrimino

handleWorld :: Event -> World -> World
handleWorld (EventKey (SpecialKey KeyDown) Down _ _)  (World field) = World (tryMove DownDir  field)
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _)  (World field) = World (tryMove LeftDir  field)
handleWorld (EventKey (SpecialKey KeyRight) Down _ _) (World field) = World (tryMove RightDir field)
handleWorld (EventKey (SpecialKey KeyUp) Down _ _)    (World field) = World (dropTetrimino    field)
handleWorld (EventKey (Char a) Down _ _)              (World field)
  | elem a ['z', 'Z', 'я', 'Я']                                     = World (tryRotateLeft    field)
handleWorld (EventKey (Char d) Down _ _)              (World field)
  | elem d ['x', 'X', 'ч', 'Ч']                                     = World (tryRotateRight   field)
handleWorld _                                         world         = world


tetrisActivity :: IO ()
tetrisActivity = play displayMode backgroundColor fps (initialWorld rand) drawWorld handleWorld updateWorld
  where
    displayMode     = (InWindow "Tetris" (300, 460) (100, 100))
    fps             = 5
    backgroundColor = makeColor (245/255) (245/255) (1.0) 0
    rand            = randoms (mkStdGen 0) :: [Int]