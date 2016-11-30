{-# LANGUAGE OverloadedStrings #-}

module Tetris where
  
import CodeWorld
import System.Random
import Data.List

data State = S Double [[Coord]] [Coord] [Coord] [Integer]
-- Double is previous time
-- [[Coord]] list of coord of all falling coord
-- [Coord] is currently falling list
-- [Coord] is currently static blocks
-- [Integer] is an infinite list of random numbers to choose blocks from

--main :: IO ()
--main = interactionOf initialState handleTime handleEvent drawState

-- initialState = S 0 [[C 0 0]] [C 0 8] [C 6 6, C 5 5, C 0 0] (randoms $ mkStdGen 0)
mkInitialState blocks = S 0 blocks [C 0 8] wallCoord (randoms $ mkStdGen 0)

--all the wallCoordinates
wallCoord :: [Coord]
wallCoord = add [] (-9)
  where add list 9 = C 9 9 : C (-9) (-9) : list
        add list n = C 9 n : C n 9 : C n (-9) : C (-9) n : add list (n+1)

handleTime :: Double -> State -> State
handleTime _ (S 3 fallingList falling static _) = 
  if canMove falling static D
    then S 0 fallingList (map (move D) falling) static []
    else S 0 fallingList [C 0 8] (falling ++ static) []
handleTime _ (S n fallingList falling static _) = S (n+1) fallingList (if canMove falling static D then (map (move D) falling) else falling) static []


--takes in state and moves to next state based on Direction
-- updateState :: Direction -> State -> State
-- updateState direction state = if canMove state then move state else state

-- canMove :: Direction -> State -> Bool
-- canMove direction (S _ _ falling static _) = intersect (map (move direction) falling) (static ++ wallCoord) == []
-- takes a list of falling and static blocks and check if the falling blocks can move in the Direction anymore
canMove :: [Coord] -> [Coord] -> Direction -> Bool
canMove falling static direction = intersect newfalling (static ++ wallCoord) == []
  where newfalling = map (move direction) falling

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == "Right" = moveFalling R s
    | key == "Left"    = moveFalling L s
handleEvent _ c = c

move :: Direction -> Coord -> Coord
move U (C x y) = C x (y + 1)
move L (C x y) = C (x - 1) y
move R (C x y) = C (x + 1) y
move D (C x y) = C x (y - 1)

moveFalling :: Direction -> State -> State
moveFalling d (S time fallingList falling static rands) = (S time fallingList (map (move d) falling) static rands)

drawState :: State -> Picture
drawState (S _ _ falling static _) = pictureOfBoard falling static

data Coord = C Integer Integer

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2

instance Show Coord where
  show (C x1 y1) = "C " ++ show x1 ++ " " ++ show y1

-- instance Show Prediction where
  -- show (Prediction a b c) = show a ++ "-" ++ show b ++ "-" ++ show c

data Direction = R | U | L | D deriving Eq

data Tile = Box | Blank | Wall deriving Eq

--generates the next block randomly, and updates state accordingly
generateBlock :: State -> (State, [Coord])
generateBlock (S time fallingList falling static rands) = 
    let block = fallingList !! (fromIntegral $ head $ rands) in
    let state = S time fallingList falling static (tail rands) in
    (state, block)

--drawing
wall, box :: Picture
box = colored brown (solidRectangle 1 1)
wall =    colored (grey 0.4) (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Box     = box
drawTile Blank   = blank
drawTile Wall    = wall

pictureOfBoard :: [Coord] -> [Coord] -> Picture
pictureOfBoard falling static = draw21times (\r -> draw21times (\c -> drawTileAt (board falling static (C r c)) (C r c)))
  where board :: [Coord] -> [Coord] -> Coord -> Tile
        board staticBoxes fallingBoxes (C x y)
          | abs x == 9 || abs y == 9 = Wall
          | elem (C x y) staticBoxes = Box 
          | elem (C x y) fallingBoxes = Box 
          | otherwise          = Blank

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-9)
  where
    go :: Integer -> Picture
    go 10 = blank
    go n  = something n & go (n+1)

drawTileAt :: Tile -> Coord -> Picture
drawTileAt tile c = atCoord c (drawTile tile)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic