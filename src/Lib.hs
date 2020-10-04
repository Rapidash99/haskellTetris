module Lib
    ( tetrisActivity
    ) where

-- {-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss (play, display)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact
import Data.List 

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

-- | Player score. Not implemented in MVP
--type Score = Int

-- | Game speed. Not implemented in MVP
--type Speed = Double

-- | Tetrimino direction
data Direction = UpDir | LeftDir | RightDir | DownDir

-- | Tetrimino type
data TetriminoType = J | I | O | L | Z | T | S | FreeCell | Wall

-- | Tetrimino
data Tetrimino = Tetrimino
  { type'        :: TetriminoType
  , coords     :: [Coords]
  }


-- | Field
data Field = Field
  { size             :: Size
  , cells            :: [[Cell]]
  , currentTetrimino :: Tetrimino
--  , nextTetriminoes  :: (Tetrimino, Tetrimino)
  }

-- | World
data World = World
  { field :: Field
  , time :: Float
  --, score :: Score
  --, speed :: Speed
  }


-- | Draw functions:


drawCell :: Cell -> Picture
drawCell ((x, y), color') = translate (fromIntegral x) (fromIntegral y) (color color' (rectangleSolid 0.95 0.95))
  
drawTetrimino :: Tetrimino -> Picture
drawTetrimino (Tetrimino type' coords) =
  blank -- to implement

drawCells :: [Cell] -> Picture
drawCells cells = pictures (map drawCell cells)

drawField :: Field -> Picture
drawField (Field _ cells currentTetrimino) = drawCells (concat cells) <> drawTetrimino currentTetrimino

drawWorld :: World -> Picture
drawWorld (World field _) = scale 20 20 (drawField field)


-- | Translation functions:

colorToType :: Color -> TetriminoType
colorToType color
  | color == white  = FreeCell
  | color == blue   = J
  | color == cyan   = I
  | color == yellow = O
  | color == orange = L
  | color == red    = Z
  | color == violet = T
  | color == green  = S
  | color == black  = Wall


typeToColor :: TetriminoType -> Color
typeToColor FreeCell = white
typeToColor J        = blue
typeToColor I        = cyan
typeToColor O        = yellow
typeToColor L        = orange
typeToColor Z        = red
typeToColor T        = violet
typeToColor S        = green
typeToColor Wall     = black

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

-- | returns all cells that tetrimino occupies (is not needed anymore as we keep all the cells initially)
--tetriminoCells :: Tetrimino -> [Cell]
--tetriminoCells (Tetrimino type' coords) = [] -- to implement


-- | Update functions:

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
canMove direction (Field size cells currentTetrimino@(Tetrimino type' coords)) = can
  where
    can             = notOutOfBorders && notIntersects
    notOutOfBorders = not (areOutOfBorders newCoords size)
    notIntersects   = not (doesIntersects fieldCoords newCoords)
    fieldCoords     = map fst (concat cells)
    newCoords       = map (\(x, y) -> (x + plusX, y + plusY)) coords
    (plusX, plusY)  = dirToCoords direction

-- | moves current tetrimino (only to use in function tryMove)
move :: Direction -> Field -> Field
move direction (Field size cells (Tetrimino type' coords)) = newField
  where
    newField       = Field size cells newTetrimino
    newTetrimino   = Tetrimino type' newCoords
    newCoords      = map (\(x, y) -> (x + plusX, y + plusY)) coords
    (plusX, plusY) = dirToCoords direction

-- | rotates current tetrimino by 90° left if can
tryRotateLeft :: Field -> Field
tryRotateLeft tetrimino = tetrimino -- to implement

-- | rotates current tetrimino by 90° right if can
tryRotateRight :: Field -> Field
tryRotateRight tetrimino = tetrimino -- to implement

-- | checks if you can rotate current tetrimino by 90° left
canRotateLeft :: Field -> Bool
canRotateLeft tetrimino = True -- to implement

-- | checks if you can rotate current tetrimino by 90° right
canRotateRight :: Field -> Bool
canRotateRight tetrimino = True -- to implement

-- | rotates current tetrimino by 90° left (only to use in function tryRotateLeft)
rotateLeft :: Field -> Field
rotateLeft tetrimino = tetrimino -- to implement

-- | rotates current tetrimino by 90° right (only to use in function tryRotateRight)
rotateRight :: Field -> Field
rotateRight tetrimino = tetrimino -- to implement

-- | returns the field without completed rows
eliminateRows :: Field -> Field
eliminateRows field@(Field _ cells _) = field -- to implement

-- | tries to remove a certain row in a field
tryEliminateRow :: Int -> Field -> Field
tryEliminateRow row field = case can of
  True -> eliminateRow row field
  False -> field
  where
    can = canEliminateRow row field

-- | checks if you can remove a certain row in a field
canEliminateRow :: Int -> Field -> Bool
canEliminateRow row (Field _ cells _) = case ((length cells) - row > 0) of
  True -> isRowFull (cells !! row)
  False -> False

-- | removes a certain row in a field (only to use in function tryEliminateRow)
eliminateRow :: Int -> Field -> Field
eliminateRow row field@(Field _ cells _) = field -- to implement

-- | updates the field when tetrimino lays down
layTetrimino :: Field -> Field
layTetrimino field@(Field size cells currentTetrimino@(Tetrimino type' coords)) = field
--  where
--    newField = Field size newCells newTetrimino
--    newTetrimino = getRandomTetrimino
--    newCells = mergeWithTetrimino
--    eliminated = eliminateRows _

mergeWithTetrimino :: [[Cell]] -> Tetrimino -> [[Cell]]
mergeWithTetrimino [] _ = []
--mergeWithTetrimino (row: rows) tetrimino = newRow ++ mergeWithTetrimino rows tetrimino
--  where
--    newRow = map (\cell -> mergeCell cell tetrimino) row

mergeCell :: Cell -> Tetrimino -> Cell
mergeCell cell@(coordinate, color) (Tetrimino type' coords) = case (elem coordinate coords) of
  True  -> (coordinate, typeToColor type')
  False -> cell

-- | Helper functions:

-- | generates random tetrimino
getRandomTetrimino :: Tetrimino
getRandomTetrimino = Tetrimino O [(0, 0), (1, 0), (1, 1), (0, 1)] -- to implement

-- | checks if coordinates intersects another ones
-- hint: use concat to reduce [[Cell]] to [Cell]
doesIntersects :: [Coords] -> [Coords] -> Bool
doesIntersects coords [] = False
doesIntersects coords (c:cs) = (elem c coords) || (doesIntersects cs coords)

-- | checks if coordinate is out of borders
isOutOfBorders :: Coords -> Size -> Bool
isOutOfBorders (x, y) (sizeX, sizeY) = x < 0 || x >= sizeX || y < 0 || y >= sizeY

-- | checks if coordinates are out of borders
areOutOfBorders :: [Coords] -> Size -> Bool
areOutOfBorders coords size = any (\coordinate -> isOutOfBorders coordinate size) coords

-- | checks if given cell is occupied
isCellOccupied :: Cell -> Bool
isCellOccupied (_, color) = color == white

-- | checks if in a given row all cells are occupied
isRowFull :: [Cell] -> Bool
isRowFull cells = all isCellOccupied cells

-- | checks if given row has no occupied cells
isRowFree :: [Cell] -> Bool
isRowFree cells = all (not . isCellOccupied) cells

--
-- | Initial objects generators:

--g :: (Int, Int) -> (Int, Int) -> [Cell]
--g (curX, curY) (maxX, maxY) = ((curX, curY), 0): (g (curX + 1, curY) (maxX, maxY))

initialCells :: (Int, Int) -> [[Cell]]
initialCells (x, y) =  
  [
  [((0, 0), white),((1, 0), white),((2, 0), white),((3, 0), white),((4, 0), white),((5, 0), white),((6, 0), white),((7, 0), white),((8, 0), white),((9, 0), white)],
  [((0, 1), white),((1, 1), white),((2, 1), white),((3, 1), white),((4, 1), white),((5, 1), white),((6, 1), white),((7, 1), white),((8, 1), white),((9, 1), white)],
  [((0, 2), white),((1, 2), white),((2, 2), white),((3, 2), white),((4, 2), white),((5, 2), white),((6, 2), white),((7, 2), white),((8, 2), white),((9, 2), white)],
  [((0, 3), white),((1, 3), white),((2, 3), white),((3, 3), white),((4, 3), white),((5, 3), white),((6, 3), white),((7, 3), white),((8, 3), white),((9, 3), white)],
  [((0, 4), white),((1, 4), white),((2, 4), white),((3, 4), white),((4, 4), white),((5, 4), white),((6, 4), white),((7, 4), white),((8, 4), white),((9, 4), white)],
  [((0, 5), white),((1, 5), white),((2, 5), white),((3, 5), white),((4, 5), white),((5, 5), white),((6, 5), white),((7, 5), white),((8, 5), white),((9, 5), white)],
  [((0, 6), white),((1, 6), white),((2, 6), white),((3, 6), white),((4, 6), white),((5, 6), white),((6, 6), white),((7, 6), white),((8, 6), white),((9, 6), white)],
  [((0, 7), white),((1, 7), white),((2, 7), white),((3, 7), white),((4, 7), white),((5, 7), white),((6, 7), white),((7, 7), white),((8, 7), white),((9, 7), white)],
  [((0, 8), white),((1, 8), white),((2, 8), white),((3, 8), white),((4, 8), white),((5, 8), white),((6, 8), white),((7, 8), white),((8, 8), white),((9, 8), white)],
  [((0, 9), white),((1, 9), white),((2, 9), white),((3, 9), white),((4, 9), white),((5, 9), white),((6, 9), white),((7, 9), white),((8, 9), white),((9, 9), white)],
  [((0, 10), white),((1, 10), white),((2, 10), white),((3, 10), white),((4, 10), white),((5, 10), white),((6, 10), white),((7, 10), white),((8, 10), white),((9, 10), white)],
  [((0, 11), white),((1, 11), white),((2, 11), white),((3, 11), white),((4, 11), white),((5, 11), white),((6, 11), white),((7, 11), white),((8, 11), white),((9, 11), white)],
  [((0, 12), white),((1, 12), white),((2, 12), white),((3, 12), white),((4, 12), white),((5, 12), white),((6, 12), white),((7, 12), white),((8, 12), white),((9, 12), white)],
  [((0, 13), white),((1, 13), white),((2, 13), white),((3, 13), white),((4, 13), white),((5, 13), white),((6, 13), white),((7, 13), white),((8, 13), white),((9, 13), white)],
  [((0, 14), white),((1, 14), white),((2, 14), white),((3, 14), white),((4, 14), white),((5, 14), white),((6, 14), white),((7, 14), white),((8, 14), white),((9, 14), white)],
  [((0, 15), white),((1, 15), white),((2, 15), white),((3, 15), white),((4, 15), white),((5, 15), white),((6, 15), white),((7, 15), white),((8, 15), white),((9, 15), white)],
  [((0, 16), white),((1, 16), white),((2, 16), white),((3, 16), white),((4, 16), white),((5, 16), white),((6, 16), white),((7, 16), white),((8, 16), white),((9, 16), white)],
  [((0, 17), white),((1, 17), white),((2, 17), white),((3, 17), white),((4, 17), white),((5, 17), white),((6, 17), white),((7, 17), white),((8, 17), white),((9, 17), white)],
  [((0, 18), white),((1, 18), white),((2, 18), white),((3, 18), white),((4, 18), white),((5, 18), white),((6, 18), white),((7, 18), white),((8, 18), white),((9, 18), white)],
  [((0, 19), white),((1, 19), white),((2, 19), white),((3, 19), white),((4, 19), white),((5, 19), white),((6, 19), white),((7, 19), white),((8, 19), white),((9, 19), white)]
  ]


initialField :: (Int, Int) -> Field
initialField size = Field
  { size             = size
  , cells            = initialCells size
  , currentTetrimino = getRandomTetrimino
  }

initialWorld :: World
initialWorld = World (initialField (10, 20)) 0


-- | Game handling functions

updateWorld :: Float -> World -> World
updateWorld dt world@(World field time) = case (dt - time > 1) of
  True -> World (tryMove DownDir field) (time + 1)
  False -> world

handleWorld :: Event -> World -> World
handleWorld (EventKey (SpecialKey KeyDown) _ _ _)  world@(World field time) = World (tryMove DownDir  field) time
handleWorld (EventKey (SpecialKey KeyLeft) _ _ _)  world@(World field time) = World (tryMove LeftDir  field) time
handleWorld (EventKey (SpecialKey KeyRight) _ _ _) world@(World field time) = World (tryMove RightDir field) time
handleWorld (EventKey (SpecialKey KeyUp) _ _ _)    world@(World field time) = World (tryMove UpDir    field) time
handleWorld (EventKey (Char 'A') _ _ _)            world@(World field time) = World (tryRotateLeft    field) time
handleWorld (EventKey (Char 'D') _ _ _)            world@(World field time) = World (tryRotateRight   field) time
handleWorld _                                      world                    = world


tetrisActivity :: IO ()
tetrisActivity = play displayMode backgroundColor fps initialWorld drawWorld handleWorld updateWorld
--tetrisActivity = display displayMode backgroundColor (drawWorld initialWorld)
  where
    displayMode = (InWindow "Tetris" (800, 800) (100, 100))
--    displayMode = FullScreen
    fps = 30
    backgroundColor = cyan