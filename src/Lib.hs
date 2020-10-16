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

-- | Game speed
type Speed = Float

-- | Player 1 or 2
data Player = P1 | P2

-- | Tetrimino direction
data Direction = UpDir | LeftDir | RightDir | DownDir

-- | Tetrimino type
data TetriminoType = J | I | O | L | Z | T | S

-- | Game state
data State = Play | Pause | Won1 | Won2

-- | Tetrimino
data Tetrimino = Tetrimino
  { type'      :: TetriminoType
  , direction  :: Direction
  , coordinate :: Coords
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
  { field1 :: Field
  , field2 :: Field
  , speed  :: Speed
  , time   :: Float
  , state  :: State
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

drawPotentialCell :: Cell -> Picture
drawPotentialCell ((x, y), color') = translate x' y' (color color' rectangle)
  where
    x' = fromIntegral x
    y' = fromIntegral (-y)
    rectangle = rectangleWire 0.9 0.9 <> rectangleWire 0.85 0.85 <> rectangleWire 0.8 0.8

drawCells :: [Cell] -> Picture
drawCells cells = pictures (map drawCell cells)

drawFrame :: Int -> Int -> Picture
drawFrame x y = frame
  where
    x' = fromIntegral x
    y' = fromIntegral y
    rectangle = rectangleWire x' y'
    frame = translate (0.45 * x') ((-0.45 * y') - 0.5) rectangle

drawTetrimino :: Field -> Picture
drawTetrimino field = tetriminoPicture
  where
    Field size _ tetrimino _ _ _ = field
    cells            = tetriminoToCells tetrimino
    cellsInside      = filter (\(coordinate, _) -> not (isOutOfBorders coordinate size)) cells
    cellsPictures    = map drawCell cellsInside
    tetriminoPicture = pictures cellsPictures

drawNextTetrimino :: Tetrimino -> Picture
drawNextTetrimino tetrimino = textNext <> tetriminoPicture
  where
    cells            = tetriminoToCells tetrimino
    cellsPictures    = map drawCell cells
    tetriminoPicture = translate 12 4 (pictures cellsPictures)
    textNext         = translate 11 2 (scale 0.015 0.015 (text ("Next: ")))

drawPotentialTetrimino :: Field -> Picture
drawPotentialTetrimino field = tetriminoPicture
  where
    Field size _ _ _ _ _ = field
    potentialTetrimino = dropTetrimino field
    cells              = tetriminoToCells potentialTetrimino
    cellsInside        = filter (\(coordinate, _) -> not (isOutOfBorders coordinate size)) cells
    cellsPictures      = map drawPotentialCell cellsInside
    tetriminoPicture   = pictures cellsPictures

drawField :: Field -> Picture
drawField field
   = drawFrame x y
  <> drawCells (concat cells)
  <> drawTetrimino field
  <> drawPotentialTetrimino field
  <> (scale 0.007 0.007 (drawScore score))
  <> scale 0.5 0.5 (drawNextTetrimino nextTetrimino)
  where
    Field (x, y) cells _ _ score nextTetrimino = field

drawWonField :: Field -> Picture
drawWonField field = fieldPicture <> textPicture <> drawRestartTipText
  where
    fieldPicture    = drawField field
    text1           = text "YOU WON"
    scaledText1     = scale 0.02 0.02 text1
    translatedText1 = translate (-2) (-6)  scaledText1
    textPicture     = boldText translatedText1

drawLostField :: Field -> Picture
drawLostField field = fieldPicture <> textPicture <> drawRestartTipText
  where
    fieldPicture    = drawField field
    text1           = text "YOU DIED"
    scaledText1     = scale 0.02 0.02 text1
    translatedText1 = translate (-2) (-6)  scaledText1
    textPicture     = boldText translatedText1

drawPausedTipText :: Picture
drawPausedTipText = whiteBackground <> textPicture
  where
    text1           = text "Paused"
    text2           = text "Press Space"
    text3           = text "to continue"
    scaledText1     = boldText (scale 0.03 0.03 text1)
    scaledText2     = scale 0.015 0.015 text2
    scaledText3     = scale 0.015 0.015 text3
    translatedText1 = translate 5.5 (-4)  scaledText1
    translatedText2 = translate 6 (-9)  scaledText2
    translatedText3 = translate 6.5 (-12) scaledText3
    textPicture     = translatedText1 <> translatedText2 <> translatedText3
    whiteBackground = color white (rectangleSolid 1000 1000)

drawRestartTipText :: Picture
drawRestartTipText = textPicture
  where
    text1           = text "Press Space"
    text2           = text "to restart"
    scaledText1     = scale 0.01 0.01 text1
    scaledText2     = scale 0.01 0.01 text2
    translatedText1 = translate   1  (-9)  scaledText1
    translatedText2 = translate   1  (-12) scaledText2
    textPicture     = boldText (translatedText1 <> translatedText2)

drawWorld :: World -> Picture
drawWorld (World field1 field2 _ _ state) = worldPicture
  where
    field1Picture = case state of
      Play  -> drawField       field1
      Pause -> drawPausedTipText
      Won1  -> drawWonField    field1
      Won2  -> drawLostField   field1
    field2Picture = case state of
      Play  -> drawField       field2
      Pause -> blank
      Won1  -> drawLostField   field2
      Won2  -> drawWonField    field2
    scaled1      = scale 20 20 field1Picture
    scaled2      = scale 20 20 field2Picture
    translated1  = translate (-235) 170 scaled1
    translated2  = translate 65 170 scaled2
    worldPicture = translated1 <> translated2

---------------------------------------------------------------
-- | Translation functions:
---------------------------------------------------------------

colorToType :: Color -> TetriminoType
colorToType color'
  | color' == dark blue   = J
  | color' == dark cyan   = I
  | color' == dark yellow = O
  | color' == orange = L
  | color' == dark red    = Z
  | color' == violet = T
  | color' == dark green  = S
  | otherwise = O

typeToColor :: TetriminoType -> Color
typeToColor J = dark blue
typeToColor I = dark cyan
typeToColor O = dark yellow
typeToColor L = orange
typeToColor Z = dark red
typeToColor T = violet
typeToColor S = dark green

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

tetriminoCoords :: Tetrimino -> [Coords]
tetriminoCoords tetrimino = coords
  where
    Tetrimino type' direction coordinate = tetrimino
    (x, y) = coordinate
    coords = case type' of
      O -> [coordinate, (x + 1, y), (x + 1, y + 1), (x, y + 1)]
      J -> case direction of
        UpDir    -> [coordinate, (x, y + 1), (x, y - 1), (x + 1, y - 1)]
        DownDir  -> [coordinate, (x, y - 1), (x, y + 1), (x - 1, y + 1)]
        RightDir -> [coordinate, (x - 1, y), (x + 1, y), (x + 1, y + 1)]
        LeftDir  -> [coordinate, (x + 1, y), (x - 1, y), (x - 1, y - 1)]
      I -> case direction of
        UpDir    -> [coordinate, (x, y + 1), (x, y - 1), (x, y - 2)]
        DownDir  -> [coordinate, (x, y - 1), (x, y + 1), (x, y + 2)]
        RightDir -> [coordinate, (x - 1, y), (x + 1, y), (x + 2, y)]
        LeftDir  -> [coordinate, (x + 1, y), (x - 1, y), (x - 2, y)]
      L -> case direction of
        UpDir    -> [coordinate, (x, y + 1), (x, y - 1), (x - 1, y - 1)]
        DownDir  -> [coordinate, (x, y - 1), (x, y + 1), (x + 1, y + 1)]
        RightDir -> [coordinate, (x - 1, y), (x + 1, y), (x + 1, y - 1)]
        LeftDir  -> [coordinate, (x + 1, y), (x - 1, y), (x - 1, y + 1)]
      Z -> case direction of
        UpDir    -> [coordinate, (x, y + 1), (x + 1, y), (x + 1, y - 1)]
        DownDir  -> [coordinate, (x, y - 1), (x - 1, y), (x - 1, y + 1)]
        RightDir -> [coordinate, (x - 1, y), (x, y + 1), (x + 1, y + 1)]
        LeftDir  -> [coordinate, (x + 1, y), (x, y - 1), (x - 1, y - 1)]
      T -> case direction of
        UpDir    -> [coordinate, (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]
        DownDir  -> [coordinate, (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]
        RightDir -> [coordinate, (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)]
        LeftDir  -> [coordinate, (x - 1, y - 1), (x - 1, y), (x - 1, y + 1)]
      S -> case direction of
        UpDir    -> [coordinate, (x, y + 1), (x - 1, y), (x - 1, y - 1)]
        DownDir  -> [coordinate, (x, y - 1), (x + 1, y), (x + 1, y + 1)]
        RightDir -> [coordinate, (x - 1, y), (x, y - 1), (x + 1, y - 1)]
        LeftDir  -> [coordinate, (x + 1, y), (x, y + 1), (x - 1, y + 1)]

tetriminoToCells :: Tetrimino -> [Cell]
tetriminoToCells tetrimino = map (\coordinate -> (coordinate, color')) coords
  where
    Tetrimino type' _ _ = tetrimino
    coords = tetriminoCoords tetrimino
    color' = typeToColor type'

---------------------------------------------------------------
-- | Update functions:
---------------------------------------------------------------

-- | moves current tetrimino if can
-- if cannot move down, updates the field
tryMove :: Player -> Direction -> World -> World
tryMove player direction world = newWorld
  where
    World field1 field2 speed time state = world
    field = case player of
      P1 -> field1
      P2 -> field2
    can      = canMove direction field
    newField = move direction field
    newWorld = case can of
      True  -> case player of
        P1 -> World newField field2 speed time state
        P2 -> World field1 newField speed time state
      False -> case direction of
        DownDir -> layTetrimino player world
        _       -> world


-- | checks if you can move current tetrimino
canMove :: Direction -> Field -> Bool
canMove direction field = can
  where
    Field size cells tetrimino _ _ _ = field
    Tetrimino type' direction' (x, y) = tetrimino

    can             = notOutOfBorders && notIntersects
    notOutOfBorders = not (areOutOfLowerBorders newCoords size)
    notIntersects   = not (doesIntersects newCoords (concat cells))
    newCoordinate   = (x + plusX, y + plusY)
    newTetrimino    = Tetrimino type' direction' newCoordinate
    newCoords       = tetriminoCoords newTetrimino
    (plusX, plusY)  = dirToCoords direction

-- | moves current tetrimino without any check
move :: Direction -> Field -> Field
move direction field = newField
  where
    Field size cells tetrimino rand score nextTetrimino = field
    Tetrimino type' direction' (x, y)                   = tetrimino

    newField       = Field size cells newTetrimino rand score nextTetrimino
    newTetrimino   = Tetrimino type' direction' newCoordinate
    newCoordinate  = (x + plusX, y + plusY)
    (plusX, plusY) = dirToCoords direction

-- | checks if you can move tetrimino in any direction and returns the list of directions
dirsCanMove :: Field -> [Direction]
dirsCanMove field = dirs
  where
    canLeft  = canMove LeftDir  field
    canRight = canMove RightDir field
    canUp    = canMove UpDir    field
    canDown  = canMove DownDir  field

    dirsLeft  = case canLeft of
      True  -> [LeftDir]
      False -> []
    dirsRight = dirsLeft  ++ case canRight of
      True  -> [RightDir]
      False -> []
    dirsUp    = dirsRight ++ case canUp of
      True  -> [UpDir]
      False -> []
    dirs      = dirsUp    ++ case canDown of
      True  -> [DownDir]
      False -> []

-- | drops current tetrimino of given player down
dropPlayerTetrimino :: Player -> World -> World
dropPlayerTetrimino player world = newWorld
  where
    World field1 field2 speed time state = world
    newField = case player of
      P1 -> scorePlus 10 field1
      P2 -> scorePlus 10 field2
    newWorld = case player of
      P1 -> recDrop P1 (World newField field2 speed time state)
      P2 -> recDrop P2 (World field1 newField speed time state)

-- | recursive function to drop tetrimino down
recDrop :: Player -> World -> World
recDrop player world = newWorld
  where
    World field1 field2 speed time state = world
    field = case player of
      P1 -> field1
      P2 -> field2
    can      = canMove DownDir field
    newField = move DownDir field
    newWorld = case can of
      True  -> case player of
        P1 -> recDrop P1 (World newField field2 speed time state)
        P2 -> recDrop P2 (World field1 newField speed time state)
      False -> tryMove player DownDir world

-- | returns dropped tetrimino
dropTetrimino :: Field -> Tetrimino
dropTetrimino field = newTetrimino
  where
    Field _ _ currentTetrimino _ _ _ = field
    can          = canMove DownDir field
    newField     = move DownDir field
    newTetrimino = case can of
      True  -> dropTetrimino newField
      False -> currentTetrimino

-- | sums two tuples of size of 2
(+++) :: Num a => (a, a) -> (a, a) -> (a, a)
(+++) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

-- | tries to rotate tetrimino by given direction
-- LeftDir  –> 90° counterclockwise
-- RightDir -> 90° clockwise
-- If can't, tries to move by one cell in opposite direction
tryRotateTetrimino :: Direction -> Field -> Field
tryRotateTetrimino dir field = newField
  where
    Field size cells currentTetrimino rand score nextTetrimino = field
    Tetrimino type' direction coords                           = currentTetrimino

    can          = canRotateTetrimino dir field
    newField     = Field size cells newTetrimino rand score nextTetrimino
    newTetrimino = case can of
      True  -> rotateTetrimino dir currentTetrimino
      False -> case canRotateMoved of
        True  -> rotateTetrimino dir movedTetrimino
        False -> currentTetrimino
    dirs           = dirsCanMove field
    dirsCanRotate  = filter (\direction' -> canRotateTetrimino dir (Field size cells (Tetrimino type' direction (coords +++ (dirToCoords direction'))) rand score nextTetrimino)) dirs
    movedTetrimino = case dirsCanRotate of
      []              -> currentTetrimino
      (direction': _) -> Tetrimino type' direction (coords +++ (dirToCoords direction'))
    movedField     = Field size cells movedTetrimino rand score nextTetrimino
    canRotateMoved = canRotateTetrimino dir movedField

-- | checks if can rotate tetrimino by given direction
canRotateTetrimino :: Direction -> Field -> Bool
canRotateTetrimino dir field = can
  where
    Field size cells currentTetrimino _ _ _ = field

    can             = notOutOfBorders && notIntersects
    notOutOfBorders = not (areOutOfLowerBorders newCoords size)
    notIntersects   = not (doesIntersects newCoords (concat cells))
    newTetrimino    = rotateTetrimino dir currentTetrimino
    newCoords       = tetriminoCoords newTetrimino

-- | rotates tetrimino by given direction without any check
rotateTetrimino :: Direction -> Tetrimino -> Tetrimino
rotateTetrimino dir tetrimino = newTetrimino
  where
    Tetrimino type' direction coordinate = tetrimino
    newDirection = rotateDirection dir direction
    newTetrimino = Tetrimino type' newDirection coordinate

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

-- | changes state of given world to WonPn, Pn given
setWon :: Player -> World -> World
setWon player world = newWorld
  where
    World field1 field2 speed time _ = world
    newWorld = case player of
      P1 -> World field1 field2 speed time Won1
      P2 -> World field1 field2 speed time Won2

-- | lays tetrimino down and updates the field correspondingly
layTetrimino :: Player -> World -> World
layTetrimino player world = newWorld
  where
    World field1 field2 speed time state = world
    Field size cells currentTetrimino rand score nextTetrimino =
      case player of
        P1 -> field1
        P2 -> field2
    (first, newRand)   = splitAt 1 rand
    randInt            = head first
    newCells           = mergeAllWithTetrimino cells currentTetrimino
    fieldWithTetrimino = Field size newCells nextTetrimino newRand score (getRandomTetrimino randInt)
    eliminatedField    = eliminateRows fieldWithTetrimino
    newWorld           = case (isGameOver eliminatedField) of
      True  -> case player of
        P1 -> World field1 field2 speed time Won2
        P2 -> World field1 field2 speed time Won1
      False -> case player of
        P1 -> World eliminatedField field2 speed time state
        P2 -> World field1 eliminatedField speed time state

-- | changes colors of cells at the place of tetrimino
mergeAllWithTetrimino :: [[Cell]] -> Tetrimino -> [[Cell]]
mergeAllWithTetrimino [] _ = []
mergeAllWithTetrimino (row: rows) tetrimino = [newRow] ++ mergeAllWithTetrimino rows tetrimino
  where
    newRow = map (\cell -> mergeCell cell tetrimino) row

-- | changes color of cell if there is a tetrimino
mergeCell :: Cell -> Tetrimino -> Cell
mergeCell cell tetrimino = newCell
  where
    (coordinate, _)               = cell
    Tetrimino type' _ _ = tetrimino
    coords  = tetriminoCoords tetrimino
    newCell = case (elem coordinate coords) of
      True  -> (coordinate, typeToColor type')
      False -> cell

-- | increases score of given field
scorePlus :: Int -> Field -> Field
scorePlus plus field = newField
  where
    Field size cells currentTetrimino rand score nextTetrimino = field
    newField = Field size cells currentTetrimino rand newScore nextTetrimino
    newScore = score + plus

-- | increases time of given world
timePlus :: Float -> World -> World
timePlus dt world = newWorld
  where
    World field1 field2 speed time state = world
    newWorld = World field1 field2 speed newTime state
    newTime = time + dt

-- | acceleration = 0.5%
updateSpeed :: World -> World
updateSpeed world = newWorld
  where
    World field1 field2 speed time state = world
    newWorld = World field1 field2 newSpeed time state
    newSpeed = speed - (speed / 200)

---------------------------------------------------------------
-- | Helper functions:
---------------------------------------------------------------

-- | makes text bold, takes it as picture
boldText :: Picture -> Picture
boldText textLayer1 = textPicture
  where
    textLayer2      = translate (-0.1)  0     textLayer1
    textLayer3      = translate (-0.05) 0     textLayer1
    textLayer4      = translate   0.05  0     textLayer1
    textLayer5      = translate   0.1   0     textLayer1
    textLayer6      = translate   0     0.05  textLayer1
    textLayer7      = translate   0   (-0.05) textLayer1
    textPicture     = textLayer1 <> textLayer2 <> textLayer3 <> textLayer4 <> textLayer5 <> textLayer6 <> textLayer7

-- | generates random tetrimino
getRandomTetrimino :: Int -> Tetrimino
getRandomTetrimino rand = tetrimino
  where
    tetrimino = case (rand `mod` 7) of
      1 -> Tetrimino J DownDir (5, -1)
      2 -> Tetrimino I DownDir (5, -2)
      3 -> Tetrimino L DownDir (5, -1)
      4 -> Tetrimino Z DownDir (5, -1)
      5 -> Tetrimino T DownDir (5, -1)
      6 -> Tetrimino S DownDir (5, -1)
      _ -> Tetrimino O DownDir (5, -1)

-- | checks if game is over by checking if you can place a new tetrimino
isGameOver :: Field -> Bool
isGameOver field = is
  where
    Field _ cells tetrimino _ _ _    = field
    coords = tetriminoCoords tetrimino
    is = doesIntersects coords (concat cells)

-- | checks if tetrimino coords intersects with cells
doesIntersects :: [Coords] -> [Cell] -> Bool
doesIntersects _ [] = False
doesIntersects [] _ = False
doesIntersects (c:cs) cells = does
  where
    does           = (elem c nonWhiteCoords) || (doesIntersects cs cells)
    nonWhite       = filter (\(_, color') -> color' /= white) cells
    nonWhiteCoords = map fst nonWhite

-- | checks if coordinate is out of borders
isOutOfBorders :: Coords -> Size -> Bool
isOutOfBorders (x, y) (sizeX, sizeY) = x < 0 || x >= sizeX || y < 0 || y >= sizeY

-- | checks if coordinate is out of lower borders (left, down, right)
isOutOfLowerBorders :: Coords -> Size -> Bool
isOutOfLowerBorders (x, y) (sizeX, sizeY) = x < 0 || x >= sizeX || y >= sizeY

-- | checks if coordinates are out of borders
areOutOfLowerBorders :: [Coords] -> Size -> Bool
areOutOfLowerBorders coords size = any (\coordinate -> isOutOfLowerBorders coordinate size) coords

-- | checks if given cell is occupied
isCellOccupied :: Cell -> Bool
isCellOccupied (_, color') = color' /= white

-- | checks if in a given row all cells are occupied
isRowFull :: [Cell] -> Bool
isRowFull cells = all isCellOccupied cells

-- | checks if given row has no occupied cells
isRowFree :: [Cell] -> Bool
isRowFree cells = all (not . isCellOccupied) cells

-- | rotates clockwise or counterclockwise given direction
rotateDirection :: Direction -> Direction -> Direction
rotateDirection leftOrRight coordinate = newCoordinate
  where
    newCoordinate = case leftOrRight of
      LeftDir  -> case coordinate of
        LeftDir  -> DownDir
        UpDir    -> LeftDir
        RightDir -> UpDir
        DownDir  -> RightDir
      RightDir -> case coordinate of
        LeftDir  -> UpDir
        UpDir    -> RightDir
        RightDir -> DownDir
        DownDir  -> LeftDir
      _ -> UpDir

-- | reverses given direction
reverseDirection :: Direction -> Direction
reverseDirection direction = newDirection
  where
    newDirection = case direction of
      LeftDir  -> RightDir
      RightDir -> LeftDir
      UpDir    -> DownDir
      DownDir  -> UpDir

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


-- | takes a sizes of new field and an infinite list of Ints and returns new Field
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
initialWorld rand = World field field 1 0.0 Play
  where
    field = initialField (10, 20) rand

---------------------------------------------------------------
-- | Game handling functions
---------------------------------------------------------------

updateWorld :: Float -> World -> World
updateWorld dt world = case state of
  Play -> newWorld
  _    -> world
  where
    World _ _ speed time state = world
    World movedField1 movedField2 _ _ newState = tryMove P1 DownDir (tryMove P2 DownDir world)
    newField1 = scorePlus 1 movedField1
    newField2 = scorePlus 1 movedField2
    newWorld = case (time >= speed) of
      True  -> updateSpeed (World newField1 newField2 speed 0.0 newState)
      False -> timePlus dt world


handleWorld :: Event -> World -> World
handleWorld (EventKey key Down _ _) world = case state of
  Play     -> case key of
    SpecialKey KeyUp    -> dropPlayerTetrimino P2 world
    SpecialKey KeyLeft  -> tryMove P2 LeftDir world
    SpecialKey KeyDown  -> tryMove P2 DownDir world
    SpecialKey KeyRight -> tryMove P2 RightDir world
    SpecialKey KeySpace -> World field1 field2 speed time Pause
    Char char
      | elem char ['w', 'W'] -> dropPlayerTetrimino P1 world
      | elem char ['a', 'A'] -> tryMove P1 LeftDir world
      | elem char ['s', 'S'] -> tryMove P1 DownDir world
      | elem char ['d', 'D'] -> tryMove P1 RightDir world
      | elem char ['x', 'X'] -> World (tryRotateTetrimino LeftDir  field1) field2 speed time state
      | elem char ['c', 'C'] -> World (tryRotateTetrimino RightDir field1) field2 speed time state
      | elem char ['k', 'K'] -> World field1 (tryRotateTetrimino LeftDir  field2) speed time state
      | elem char ['l', 'L'] -> World field1 (tryRotateTetrimino RightDir field2) speed time state
      | otherwise -> world
    _ -> world
  Pause    -> case key of
    SpecialKey KeySpace -> World field1 field2 speed time Play
    _ -> world
  _ -> case key of
    SpecialKey KeySpace -> initialWorld rand
    _ -> world
  where
    World field1 field2 speed time state = world
    Field _ _ _ rand _ _ = field1
handleWorld _ world = world

tetrisActivity :: IO ()
tetrisActivity = play displayMode backgroundColor fps (initialWorld rand) drawWorld handleWorld updateWorld
  where
    displayMode     = (InWindow "Tetris" (640, 480) (100, 100))
    fps             = 60
    backgroundColor = makeColor (150/255) (150/255) (150/255) 0
    rand            = randoms (mkStdGen 0) :: [Int]