{-# LANGUAGE OverloadedStrings #-}

module Tetris where
  
import CodeWorld
import System.Random

data State = S Double [[Coord]] [Coord] [Coord] [Integer]
-- Double is previous time
-- [[Coord]] list of coord of all falling coord
-- [Coord] is currently falling list
-- [Coord] is currently static blocks
-- [Integer] is an infinite list of random numbers to choose blocks from

--main :: IO ()
--main = interactionOf initialState handleTime handleEvent drawState

initialState = S 0 [[C 0 0]] [C 0 8] [] (randoms $ mkStdGen 0)
mkInitialState blocks = S 0 blocks [C 0 8] [] (randoms $ mkStdGen 0)

handleTime :: Double -> State -> State

-- handleTime _ s = moveFalling D s
handleTime double (S time fallingList falling static rands) = 
  if (double > time) then S double fallingList (map (move D) falling) static rands
    else S time fallingList falling static rands

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

data Direction = R | U | L | D deriving Eq

data Tile = Box | Blank | Wall deriving Eq

fallingBoxes :: [Coord]
fallingBoxes = [(C 1 1), (C 0 0)]

staticBoxes :: [Coord]
staticBoxes = [(C 2 2), (C 0 2)]

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
          | elem (C x y) staticBoxes = Box 
          | elem (C x y) fallingBoxes = Box 
          | abs x == 9 || abs y == 9 = Wall
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



