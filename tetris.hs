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
mkInitialState blocks = S 0 blocks [C 0 8] [] (randoms $ mkStdGen 0)

--all the wallCoordinates
wallCoord :: [Coord]
wallCoord = add [] (-9)
  where add list 9 = C 9 9 : C (-9) (-9) : list
        add list n = C 9 n : C n 9 : C n (-9) : C (-9) n : add list (n+1)

handleTime :: Double -> State -> State
handleTime _ (S 3 fallingList falling static _) = 
  if canMove (map (move D) falling) static D
    then S 0 fallingList (map (move D) falling) static []
    else S 0 fallingList [C 0 8] (removeRow falling (falling ++ static)) []
handleTime _ (S n fallingList falling static _) = S (n+1) fallingList falling static []


move :: Direction -> Coord -> Coord
move U (C x y) = C x (y + 1)
move L (C x y) = C (x - 1) y
move R (C x y) = C (x + 1) y
move D (C x y) = C x (y - 1)


handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | key == "Right" = moveFalling R s
    | key == "Left"    = moveFalling L s
handleEvent _ c = c

moveFalling :: Direction -> State -> State
moveFalling d (S time fallingList falling static rands) = (S time fallingList (moveCoords falling static d) static rands)

-- moveFalling d (S time fallingList falling static rands) = (S time fallingList (map (move d) falling) static rands)

-- takes a list of falling and static blocks and check if the falling blocks can move in the Direction anymore
moveCoords :: [Coord] -> [Coord] -> Direction -> [Coord]
moveCoords falling static direction = if canMove newFalling static direction then newFalling else falling
-- moveCoords falling static direction = newFalling
  where newFalling = map (move direction) falling

canMove :: [Coord] -> [Coord] -> Direction -> Bool
canMove falling static direction = intersect falling (static ++ wallCoord) == []

-- takes falling coords and (falling + static) coords and check if a new row has been formed
removeRow :: [Coord] -> [Coord] -> [Coord]
removeRow falling static = helper getYCoord static --newStatic--helper2 newStatic newStatic

  where newStatic = helper getYCoord static
        getYCoord :: [Integer]
        getYCoord = nub (map (\(C _ y) -> y) falling) -- get the Y coord of all the falling blocks in sorted order
        
        helper [] static = static
        helper (x:xs) static = helper (xs) (remove x static)


        -- helper2 [] static = static
        -- helper2 (x:xs) static = helper2 xs ((moveCoords [x] static D) ++ (delete x static))    --if moveCoords [x] static D then helper2 xs ((move D x) : delete x static) else helper2 xs static

        -- gets a row and removes it if full
        remove :: Integer -> [Coord] -> [Coord]
        remove row static = if isFull row static then filter (\(C _ x) -> x /= row) static else static
        isFull :: Integer -> [Coord] -> Bool
        isFull row rowCoord = and (map (\r -> elem (C r row) rowCoord) rowOfInt)
        -- getRow row = filter (\(C _ y) -> y == row) static -- check if the entire row is there
        -- isPresent :: Integer -> [Coord] -> Bool
        -- isPresent int rowCoord = elem int (map (\(C x _) -> x) rowCoord) -- checks if a single number is in the list
        
        --and (map (\r -> elem r (map (\(C x _) -> x) rowCoord)) rowOfInt)
        -- check if entire row is full

        --elem row (map (\(C x _) -> x) rowCoord) -- check if integer is in rowCoord --(map (\row -> map (\(C _ y) -> y == r) row) row)

        -- and (map (\r -> elem ()))
        --(map (\r -> map (\(C _ y) -> y == r) row) row) -- checks if entire row is filled
        rowOfInt :: [Integer]
        rowOfInt = go (-8) []
        go 8 list = 8 : list
        go n list = n : go (n + 1) list

rowOfCoord int = go (-9) []
  where go 10 list = list
        go n list = (C n int) : (go (n+1) list)

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



