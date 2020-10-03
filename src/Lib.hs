module Lib
    ( tetrisActivity
    ) where

{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}

import Graphics.Gloss (display, white, Picture (Circle), Display (InWindow))

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
-- * 0 — white  (free cell)
-- * 1 — blue   (J)
-- * 2 — cyan   (I)
-- * 3 — yellow (O)
-- * 4 — orange (L)
-- * 5 — red    (Z)
-- * 6 — purple (T)
-- * 7 — green  (S)
type Color = Int

-- | Cell of which fields are composed
type Cell = (Coords, Color)

-- | Player score. Not implemented in MVP
type Score = Int

-- | Game speed. Not implemented in MVP
type Speed = Double

-- | Tetrimino rotation angle:
--
-- * Up'
-- * Left'
-- * Down'
-- * Right'
data Angle = Up' | Direction

-- | Direction to move tetrimino
data Direction = Left' | Right' | Down'

-- | Tetrimino type
data TetriminoType = J | I | O | L | Z | T | S

-- | Tetrimino
data Tetrimino = Tetrimino
  { t        :: TetriminoType
    , angle  :: Angle
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

--drawTetrimino :: Tetrimino -> Picture
--drawTetrimino tetrimino = _

--drawCell :: Cell -> Picture
--drawCell cell = _
--  where
--    ((x, y), color) = cell

--drawField :: Field -> Picture
--drawField field = _

--drawWorld :: World -> Picture
--drawWorld World{field=field'} = drawField field'


-- | Translation functions:

dirToCoords :: Direction -> Coords
dirToCoords dir = case dir of
  Left'  -> (-1, 0)
  Right' -> (1, 0)
  Down'  -> (0, 1)

coordsToDir :: Coords -> Maybe Direction
coordsToDir coords = case coords of
  (-1, 0) -> Just Left'
  (1, 0)  -> Just Right'
  (0, 1)  -> Just Down'
  _       -> Nothing


-- | Update functions:

moveTetrimino :: Direction -> Field -> Field
moveTetrimino direction field = field -- to implement

canMoveTetrimino :: Direction -> Field -> Bool
canMoveTetrimino direction field = True -- to implement

rotateLeft :: Tetrimino -> Tetrimino
rotateLeft tetrimino = tetrimino -- to implement

rotateRight :: Tetrimino -> Tetrimino
rotateRight tetrimino = tetrimino -- to implement

--rotateSelf :: Direction -> Tetrimino -> Tetrimino
--rotateSelf tetrimino direction = tetrimino -- to implement

-- | Helper functions:

getRandomTetrimino :: Tetrimino
getRandomTetrimino = Tetrimino J Up' (0, 0)

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

tryMoveTetrimino :: Direction -> Field -> Field
tryMoveTetrimino direction field = case can of
  True -> moveTetrimino direction field
  False -> field
  where
    can = canMoveTetrimino direction field



--handleWorld :: Event -> World -> World
--handleWorld event world = _
--handleWorld (KeyPress "Down") = tryMove D
--handleWorld (KeyPress "Left") = tryMove L
--handleWorld (KeyPress "Right") = tryMove R
--handleWorld _ = tryMove Fix

tetrisActivity :: IO ()
tetrisActivity = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)
--tetrisActivity = activityOf initialWorld handleWorld drawWorld