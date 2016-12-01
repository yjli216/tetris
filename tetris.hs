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
handleTime _ (S 10 fallingList falling static _) = 
  if canMove falling static D
    then S 0 fallingList (map (move D) falling) static []
    else S 0 fallingList [C 0 8] (falling ++ static) []
handleTime _ (S n fallingList falling static _) = S (n+1) fallingList (if canMove falling static D then (map (move D) falling) else falling) static []

-- takes a list of falling and static blocks and check if the falling blocks can move in the Direction anymore
canMove :: [Coord] -> [Coord] -> Direction -> Bool
canMove falling static direction = intersect newfalling (static ++ wallCoord) == []
  where newfalling = map (move direction) falling

-- takes falling coords and (falling + static) coords and check if a new row has been formed
removeRow :: [Coord] -> [Coord] -> [Coord]
removeRow falling static = falling
  where getYCoord :: [Integer]
        getYCoord = sort (nub (map (\(C _ y) -> y) falling)) -- get the Y coord of all the falling blocks in sorted order
        


        -- gets a row and removes it if full
        remove :: Integer -> [Coord] -> [Coord]
        remove row static = if isFull row static then filter (\(C _ x) -> x /= row) static else static
        -- getRow row = filter (\(C _ y) -> y == row) static -- check if the entire row is there
        -- isPresent :: Integer -> [Coord] -> Bool
        -- isPresent int rowCoord = elem int (map (\(C x _) -> x) rowCoord) -- checks if a single number is in the list
        isFull :: Integer -> [Coord] -> Bool
        isFull row rowCoord = and (map (\r -> elem (C r row) rowCoord) rowOfInt)
        --and (map (\r -> elem r (map (\(C x _) -> x) rowCoord)) rowOfInt)
        -- check if entire row is full

        --elem row (map (\(C x _) -> x) rowCoord) -- check if integer is in rowCoord --(map (\row -> map (\(C _ y) -> y == r) row) row)

        -- and (map (\r -> elem ()))
        --(map (\r -> map (\(C _ y) -> y == r) row) row) -- checks if entire row is filled
        rowOfInt :: [Integer]
        rowOfInt = go (-9) []
        go 9 list = 9 : list
        go n list = n : go (n + 1) list

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