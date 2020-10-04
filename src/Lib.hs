module Lib
    ( tetrisActivity
    ) where

-- {-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss (play, display)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact

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
data TetriminoType = J | I | O | L | Z | T | S | FreeCell

-- | Tetrimino
data Tetrimino = Tetrimino
  { type'    :: TetriminoType
    , direction  :: Direction
    , coords :: Coords
  }


-- | Field
data Field = Field
  { size             :: Size
  , cells            :: [[Cell]]
  , currentTetrimino :: Tetrimino
  , nextTetriminoes  :: (Tetrimino, Tetrimino)
  }

-- | World
data World = World
  { field :: Field
  --, score :: Score
  --, speed :: Speed
  }


-- | Draw functions:

drawTetrimino :: Tetrimino -> Picture
drawTetrimino Tetrimino{type'=type'', direction=direction', coords=(x, y)} =
  blank -- to implement

drawCell :: Cell -> Picture
drawCell cell = blank -- to implement
  where
    ((x, y), color) = cell

drawField :: Field -> Picture
drawField field = blank -- to implement

drawWorld :: World -> Picture
drawWorld World{field=field'} = drawField field'


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

typeToColor :: TetriminoType -> Color
typeToColor FreeCell = white
typeToColor J        = blue
typeToColor I        = cyan
typeToColor O        = yellow
typeToColor L        = orange
typeToColor Z        = red
typeToColor T        = violet
typeToColor S        = green

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

-- | returns all cells that tetrimino occupies
tetriminoCells :: Tetrimino -> [Cell]
tetriminoCells tetrimino@(Tetrimino type' direction (x, y)) = [] -- to implement


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
      DownDir -> nextTetrimino field
      _       -> field

-- | checks if you can move current tetrimino
canMove :: Direction -> Field -> Bool
canMove direction field = True -- to implement

-- | moves current tetrimino (only to use in function tryMove)
move :: Direction -> Field -> Field
move direction field = field -- to implement

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
eliminateRows field@(Field _ cells _ _) = field -- to implement

-- | tries to remove a certain row in a field
tryEliminateRow :: Int -> Field -> Field
tryEliminateRow row field = field -- to implement

-- | checks if you can remove a certain row in a field
canEliminateRow :: Int -> Field -> Bool
canEliminateRow row field = True -- to implement

-- | removes a certain row in a field (only to use in function tryEliminateRow)
eliminateRow :: Int -> Field -> Field
eliminateRow row field = field -- to implement

-- | updates the field when tetrimino falls down
nextTetrimino :: Field -> Field
nextTetrimino field = newField
  where
    newField = field -- to implement
    --eliminated = eliminateRows _


-- | Helper functions:

-- | generates random tetrimino
getRandomTetrimino :: Tetrimino
getRandomTetrimino = Tetrimino L UpDir (0, 0) -- to implement

-- | checks if given tetrimino intersects with cells
-- hint: use concat to reduce [[Cell]] to [Cell]
doesIntersects :: Tetrimino -> [Cell] -> Bool
doesIntersects tetrimino cells = True -- to implement

-- | checks if given cell is occupied
isCellOccupied :: Cell -> Bool
isCellOccupied (_, color) = color == white

-- | checks if given row is full
isRowFull :: [Cell] -> Bool
isRowFull cells = all isCellOccupied cells

--
-- | Initial objects generators:

--g :: (Int, Int) -> (Int, Int) -> [Cell]
--g (curX, curY) (maxX, maxY) = ((curX, curY), 0): (g (curX + 1, curY) (maxX, maxY))

initialCells :: (Int, Int) -> [[Cell]]
initialCells (x, y) = [[]] -- [[((0, 0), 0), ((1, 0), 0), ((2, 0), 0), ...]] -- to implement

initialField :: (Int, Int) -> Field
initialField size = Field
  { size             = size
  , cells            = initialCells size
  , currentTetrimino = getRandomTetrimino
  , nextTetriminoes  = (getRandomTetrimino, getRandomTetrimino)
  }

initialWorld :: World
initialWorld = World {field = initialField (10, 10)}


-- | Game handling functions

updateWorld :: Float -> World -> World
updateWorld dt (World field) = World (tryMove DownDir field)

handleWorld :: Event -> World -> World
handleWorld (EventKey (SpecialKey KeyDown) _ _ _)  world@(World field) = World (tryMove DownDir field)
handleWorld (EventKey (SpecialKey KeyLeft) _ _ _)  world@(World field) = World (tryMove LeftDir field)
handleWorld (EventKey (SpecialKey KeyRight) _ _ _) world@(World field) = World (tryMove RightDir field)
handleWorld (EventKey (Char 'A') _ _ _)            world@(World field) = World (tryRotateLeft field)
handleWorld (EventKey (Char 'D') _ _ _)            world@(World field) = World (tryRotateRight field)
handleWorld _                                      world@(World field) = world


tetrisActivity :: IO ()
tetrisActivity = play displayMode backgroundColor steps initialWorld drawWorld handleWorld updateWorld
  where
    displayMode = (InWindow "Nice Window" (800, 800) (10, 10))
    steps = 1
    backgroundColor = white