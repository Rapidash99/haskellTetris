module Lib
    ( tetrisActivity
    ) where

{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}
{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

type Coords = (Int, Int)
type Score = Int
type Speed = Double

data Angle = Up' | Right' | Down' | Left'

data TetriminoType = J | I | O | L | Z | T | S

data Tetrimino = Tetrimino
  { t :: TetriminoType
    , angle :: Angle
    , coords :: Coords
  }

data Field = Field
  { cells :: [[(Coords, Bool)]]
  , currentTetrimino :: Tetrimino
  , nextTetriminoes :: (Tetrimino, Tetrimino)
  }

data World = World
  { field :: Field
  , score :: Score
  --, speed :: Speed
  }

drawTetrimino :: Tetrimino -> Picture
drawTetrimino tetrimino = _

moveTetrimino :: Tetrimino -> Tetrimino
moveTetrimino tetrimino = _

canMoveTetrimino :: Tetrimino -> Bool
canMoveTetrimino tetrimino = _

getRandomTetrimino :: Tetrimino
getRandomTetrimino = _

rotateLeft :: Tetrimino -> Tetrimino
rotateLeft tetrimino = _

rotateRight :: Tetrimino -> Tetrimino
rotateRight tetrimino = _

initialWorld :: World
drawWorld :: World -> Picture
updateWorld :: Double -> World -> World
handleWorld :: Event -> World -> World

tetrisActivity :: IO ()
tetrisActivity = activityOf initialWorld handleWorld drawWorld
