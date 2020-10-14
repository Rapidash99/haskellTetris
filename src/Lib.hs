module Lib
    ( tetrisActivity
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss (play)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact
--import Graphics.Gloss.Interface.IO.Game (playIO)
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

-- | Game speed
type Speed = Float

-- | Tetrimino direction
data Direction = UpDir | LeftDir | RightDir | DownDir

-- | Tetrimino type
data TetriminoType = J | I | O | L | Z | T | S

-- | Game state
data State = Play | Pause | GameOver

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
  , speed :: Speed
  , time  :: Float
  , state :: State
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

drawPausedField :: Field -> Picture
drawPausedField _field = scale 0.01 0.01 (text "Paused")

drawGameOverField :: Field -> Picture
drawGameOverField field = fieldPicture <> textPicture
  where
    fieldPicture    = drawField field
    text1           = text "YOU DIED"
    text2           = text "Press Space"
    text3           = text "to restart"
    scaledText1     = scale 0.02 0.02 text1
    scaledText2     = scale 0.01 0.01 text2
    scaledText3     = scale 0.01 0.01 text3
    translatedText1 = translate (-1) (-6)  scaledText1
    translatedText2 = translate   1  (-9)  scaledText2
    translatedText3 = translate   1  (-12) scaledText3
    textLayer1      = translatedText1 <> translatedText2 <> translatedText3
    textLayer2      = translate (-0.05) 0 textLayer1
    textLayer3      = translate   0.05  0 textLayer1
    textPicture     = textLayer1 <> textLayer2 <> textLayer3

drawWorld :: World -> Picture
drawWorld (World field _ _ state) = worldPicture
  where
    fieldPicture = case state of
      Play     -> drawField         field
      Pause    -> drawPausedField   field
      GameOver -> drawGameOverField field
    scaled       = scale 20 20 fieldPicture
    worldPicture = translate (-100) 180 scaled

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
tryMove :: Direction -> World -> World
tryMove direction world = newWorld
  where
    World field _ _ _ = world

    can      = canMove direction field
    newWorld = case can of
      True  -> move direction world
      False -> case direction of
        DownDir -> layTetrimino world
        _       -> world


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
move :: Direction -> World -> World
move direction world = newWorld
  where
    World field speed time state                        = world
    Field size cells tetrimino rand score nextTetrimino = field
    Tetrimino type' coords                              = tetrimino

    newWorld       = World newField speed time state
    newField       = Field size cells newTetrimino rand score nextTetrimino
    newTetrimino   = Tetrimino type' newCoords
    newCoords      = map (\(x, y) -> (x + plusX, y + plusY)) coords
    (plusX, plusY) = dirToCoords direction

-- | drops current tetrimino down
dropTetrimino :: World -> World
dropTetrimino world = newWorld
  where
    World field speed time state                        = world
    Field size cells tetrimino rand score nextTetrimino = field

    newField = Field size cells tetrimino rand (score + 10) nextTetrimino
    newWorld = recDrop (World newField speed time state)

-- | recursive function to drop tetrimino down
recDrop :: World -> World
recDrop world = newWorld
  where
    World field _ _ _= world

    can      = canMove DownDir field
    newWorld = case can of
      True  -> recDrop (move DownDir world)
      False -> tryMove DownDir world

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
    Field _ cells _ _ _ _ = field

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
incrementY (((x, y), color'): cells) = ((x, y + 1), color'): incrementY cells

-- | removes a certain row in a field (only to use in function tryEliminateRow)
eliminateRow :: [Cell] -> [[Cell]] -> [[Cell]]
eliminateRow cells rows = highPart ++ lowPart
  where
    (rows1, rows2) = break ( == cells) rows
    lowPart        = drop 1 rows2
    highPart       = newRow: (map incrementY rows1)
    newRow         = [((0, 0), white),((1, 0), white),((2, 0), white),((3, 0), white),((4, 0), white),
                      ((5, 0), white),((6, 0), white),((7, 0), white),((8, 0), white),((9, 0), white)]

-- | lays tetrimino down and updates the field correspondingly
layTetrimino :: World -> World
layTetrimino world = newWorld
  where
    World field speed time state = world
    Field size cells currentTetrimino rand score nextTetrimino = field

    (first, newRand)   = splitAt 1 rand
    randInt            = head first
    newCells           = mergeAllWithTetrimino cells currentTetrimino
    fieldWithTetrimino = Field size newCells nextTetrimino newRand score (getRandomTetrimino randInt)
    eliminatedField    = eliminateRows fieldWithTetrimino
    newWorld           = case (isGameOver eliminatedField) of
      True  -> World field speed time GameOver
      False -> World eliminatedField speed time state

-- | changes colors of cells at the place of tetrimino
mergeAllWithTetrimino :: [[Cell]] -> Tetrimino -> [[Cell]]
mergeAllWithTetrimino [] _ = []
mergeAllWithTetrimino (row: rows) tetrimino = [newRow] ++ mergeAllWithTetrimino rows tetrimino
  where
    newRow = map (\cell -> mergeCell cell tetrimino) row

-- | changes color of cell if there is a tetrimino
mergeCell :: Cell -> Tetrimino -> Cell
mergeCell cell@(coordinate, _) (Tetrimino type' coords) = case (elem coordinate coords) of
  True  -> (coordinate, typeToColor type')
  False -> cell

---------------------------------------------------------------
-- | Helper functions:
---------------------------------------------------------------

-- | generates random tetrimino
getRandomTetrimino :: Int -> Tetrimino
getRandomTetrimino rand = case (rand `mod` 7) of
  1 -> Tetrimino J [(5, 0), (5, 1), (5, 2), (4, 2)]
  2 -> Tetrimino I [(5, 0), (6, 0), (7, 0), (8, 0)]
  3 -> Tetrimino L [(5, 0), (5, 1), (5, 2), (6, 2)]
  4 -> Tetrimino Z [(5, 0), (6, 0), (6, 1), (7, 1)]
  5 -> Tetrimino T [(5, 0), (5, 1), (4, 1), (6, 1)]
  6 -> Tetrimino S [(5, 0), (5, 1), (6, 1), (6, 2)]
  _ -> Tetrimino O [(5, 0), (6, 0), (6, 1), (5, 1)]

-- | checks if game is over by checking if you can place a new tetrimino
isGameOver :: Field -> Bool
isGameOver (Field _ cells tetrimino _ _ _) = is
  where
    is = doesIntersects tetrimino (concat cells)

-- | checks if tetrimino intersects with cells
doesIntersects :: Tetrimino -> [Cell] -> Bool
doesIntersects _ [] = False
doesIntersects (Tetrimino _ []) _ = False
doesIntersects (Tetrimino type' (c: cs)) cells = does
  where
    does           = (elem c nonWhiteCoords) || (doesIntersects (Tetrimino type' cs) cells)
    nonWhite       = filter (\(_, color') -> color' /= white) cells -- O(n^2), need to fix
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
initialWorld rand = World (initialField (10, 20) rand) 1 0.0 Play

---------------------------------------------------------------
-- | Game handling functions
---------------------------------------------------------------

-- | acceleration = 0.5%
updateWorld :: Float -> World -> World
updateWorld dt world = case state of
  Play -> newWorld
  _    -> world
  where
    World field speed time state                                                    = world
    World (Field size cells currentTetrimino rand score nextTetrimino) _ _ newState = tryMove DownDir world
    newField = Field size cells currentTetrimino rand (score + 1) nextTetrimino
    newSpeed = speed - (speed / 200)
    newWorld = case (time >= speed) of
      True  -> World newField newSpeed 0.0 newState
      False -> World field speed (time + dt) state


handleWorld :: Event -> World -> World
handleWorld (EventKey key Down _ _) world = case state of
  Play     -> case key of
    SpecialKey KeyDown  -> tryMove DownDir world
    SpecialKey KeyLeft  -> tryMove LeftDir world
    SpecialKey KeyRight -> tryMove RightDir world
    SpecialKey KeyUp    -> dropTetrimino world
    SpecialKey KeySpace -> World field speed time Pause
    Char char
      | elem char ['z', 'Z'] -> World (tryRotateLeft  field) speed time state
      | elem char ['x', 'X'] -> World (tryRotateRight field) speed time state
      | otherwise -> world
    _ -> world
  Pause    -> case key of
    SpecialKey KeySpace -> World field speed time Play
    _ -> world
  GameOver -> case key of
    SpecialKey KeySpace -> initialWorld rand
    _ -> world
  where
    World field@(Field _ _ _ rand _ _) speed time state = world
handleWorld _ world = world

tetrisActivity :: IO ()
tetrisActivity = play displayMode backgroundColor fps (initialWorld rand) drawWorld handleWorld updateWorld
  where
    displayMode     = (InWindow "Tetris" (300, 460) (100, 100))
    fps             = 60
    backgroundColor = makeColor (245/255) (245/255) (1.0) 0
    rand            = randoms (mkStdGen 0) :: [Int]