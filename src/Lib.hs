module Lib
    ( tetrisActivity
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event (..), SpecialKey (..), Key (..))

-- | Coordinates:
--
-- (x, y)
type Coords = (Int, Int)

-- | Field size:
--
-- (x, y)
type Size = Coords

-- | A color:
--
-- * white  – free cell
-- * blue   – J
-- * cyan   – I
-- * yellow – O
-- * orange – L
-- * red    – Z
-- * purple – T
-- * green  – S

-- | Cell of which fields are composed
type Cell = (Coords, Color)

-- | Player score. Not implemented in MVP
type Score = Int

-- | Game speed. Not implemented in MVP
type Speed = Double

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
  | color == white = FreeCell
--  | color ==
--  | color ==
--  | color ==
--  | color ==
--  | color ==
--  | color ==
--  | color ==

dirToCoords :: Direction -> Coords
dirToCoords dir = case dir of
  LeftDir  -> (-1, 0)
  RightDir -> (1, 0)
  DownDir  -> (0, 1)
  UpDir -> (0, -1)

coordsToDir :: Coords -> Maybe Direction
coordsToDir coords = case coords of
  (-1, 0) -> Just LeftDir
  (1, 0)  -> Just RightDir
  (0, 1)  -> Just DownDir
  (0, -1) -> Just UpDir
  _       -> Nothing


-- | Update functions:

-- | moves current tetrimino if can
tryMove :: Direction -> Field -> Field
tryMove direction field = case can of
  True -> move direction field
  False -> field
  where
    can = canMove direction field

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

-- | Helper functions:

getRandomTetrimino :: Tetrimino
getRandomTetrimino = Tetrimino L UpDir (0, 0) -- to implement

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

updateWorld :: Double -> World -> World
updateWorld t world = world -- to implement

--handleWorld :: Event -> World -> World
--handleWorld (EventKey KeyDown _ _ _)  world@(field) = World (tryMove DownDir field)
--handleWorld (EventKey KeyLeft _ _ _)  world@(field) = World (tryMove LeftDir field)
--handleWorld (EventKey KeyRight _ _ _) world@(field) = World (tryMove RightDir field)
--handleWorld (EventKey 'A' _ _ _)      world@(field) = World (tryRotateLeft field)
--handleWorld (EventKey 'D' _ _ _)      world@(field) = World (tryRotateRight field)
--handleWorld _                         world@(field) = world


tetrisActivity :: IO ()
tetrisActivity = display (InWindow "Nice Window" (800, 800) (10, 10)) cyan (Circle 80)
--tetrisActivity = play displayMode backgroundColor steps initialWorld drawWorld handleWorld eatController
  where
    displayMode = (InWindow "Nice Window" (800, 800) (10, 10))
    steps = 1
    backgroundColor = white