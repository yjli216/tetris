{-# LANGUAGE OverloadedStrings #-}

module Tetris where
  
import CodeWorld
import System.Random
import Data.List
import Data.Text(pack)
import Data.Char(chr)

data State = S Double [[Coord]] [Coord] [Coord] [Integer] Integer
-- Double is previous time
-- [[Coord]] list of coord of all falling coord
-- [Coord] is currently falling list
-- [Coord] is currently static blocks
-- [Integer] is an infinite list of random numbers to choose blocks from
-- Integer is the current number of rows cleared

data Interaction world = Interaction world (Double -> world -> world) (Event -> world -> world) (world -> Picture)

--runs the interaction
runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

--makes teh game resettable
resetable :: Interaction s -> Interaction s
resetable (Interaction state handleTime handleEvent drawState) 
  = Interaction state handleTime handleEvent' drawState 
  where handleEvent' (KeyPress key) _ | key == "Esc" = state
        handleEvent' e s = handleEvent e s

--initialise the block
mkInitialState blocks = S 0 blocks (head blocks) [] (randoms $ mkStdGen 0) 0

--all the wall Coordinates (except top row)
wallCoord :: [Coord]
wallCoord = add [] (-9)
  where add list 9 = C 9 9 : C (-9) (-9) : list
        add list n = C 9 n : C n (-9) : C (-9) n : add list (n+1)

handleTime :: Double -> State -> State
handleTime _ (S 2 fallingList falling static rands score) = 
  if canMove (map (move D) falling) static D
  then S 0 fallingList (map (move D) falling) static rands score
  else
    let (newStatic, newScore) = removeRow score falling (falling ++ static) in
    S 0 fallingList (generateBlock fallingList rands) newStatic (tail rands) newScore
handleTime _ (S n fallingList falling static rands score) = S (n+1) fallingList falling static rands score

gameLost :: State -> Bool
gameLost (S _ _ _ static _ _) = filter (\(C _ y) -> y == 9) static /= []

move :: Direction -> Coord -> Coord
move U (C x y) = C x (y + 1)
move L (C x y) = C (x - 1) y
move R (C x y) = C (x + 1) y
move D (C x y) = C x (y - 1)

--shifts a block by n in direction d (used by rotate fxn)
shiftBlock :: Integer -> Direction -> [Coord] -> [Coord]
shiftBlock n U l = map (\(C x y) -> C x (y + n)) l
shiftBlock n L l = map (\(C x y) -> C (x - n) y) l
shiftBlock n R l = map (\(C x y) -> C (x + n) y) l
shiftBlock n D l = map (\(C x y) -> C x (y - n)) l

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
    | gameLost s = s
    | key == "Right" = moveFalling R s
    | key == "Left"  = moveFalling L s
    | key == "Up"    = moveFalling R $ rotateBlock False s
    | key == "Down"  = moveFalling D $ moveFalling D $ rotateBlock True s
handleEvent _ c = c

moveFalling :: Direction -> State -> State
moveFalling d (S time fallingList falling static rands score) = (S time fallingList (moveCoords falling static d) static rands score)

-- moves block if can, else don't move them
moveCoords :: [Coord] -> [Coord] -> Direction -> [Coord]
moveCoords falling static direction = if canMove newFalling static direction then newFalling else falling
  where newFalling = map (move direction) falling

-- takes a list of falling and static blocks and check if the falling blocks can move in the Direction
canMove :: [Coord] -> [Coord] -> Direction -> Bool
canMove falling static direction = intersect falling (static ++ wallCoord) == []

-- takes falling coords and (falling + static) coords and check if a new row has been formed
removeRow :: Integer -> [Coord] -> [Coord] -> ([Coord], Integer)
removeRow score falling static = helper getYCoord (static, score) where

  getYCoord :: [Integer]
  getYCoord = reverse $ nub $ map (\(C _ y) -> y) falling -- get the Y coord of all the falling blocks in reverse sorted order
  
  helper [] (static, score) = (static, score)
  helper (x:xs) (static, score) = helper xs $ remove score x static

  -- gets a row and removes it if full
  remove :: Integer -> Integer -> [Coord] -> ([Coord], Integer)
  remove score row static = if isFull row static 
    then (shiftDown row (filter (\(C _ x) -> x /= row) static), score + 1) 
    else (static, score)

  -- shifts coord down
  shiftDown :: Integer -> [Coord] -> [Coord]
  shiftDown row static = map (\(C x y) -> if y > row then C x (y-1) else C x y) static

  -- check if row is full
  isFull :: Integer -> [Coord] -> Bool
  isFull row rowCoord = and (map (\r -> elem (C r row) rowCoord) rowOfInt)

  -- generates a list of numbers of [-8 to 8]
  rowOfInt :: [Integer]
  rowOfInt = go (-8) []
  go 8 list = 8 : list
  go n list = n : go (n + 1) list

drawState :: State -> Picture
drawState s
  | gameLost s = scaled 2 2 (text "Game Over!");
drawState (S _ _ falling static _ score) = pictureOfBoard falling static score

data Coord = C Integer Integer

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2) = x1 == x2 && y1 == y2

instance Eq Coord where
  C x1 y1 == C x2 y2 = x1 == x2 && y1 == y2

instance Show Coord where
  show (C x1 y1) = "C " ++ show x1 ++ " " ++ show y1

data Direction = R | U | L | D deriving Eq

data Tile = Box | Blank | Wall deriving Eq


--generates the next block randomly, and updates state accordingly
generateBlock :: [[Coord]] -> [Integer] -> [Coord]
generateBlock _ [] = (C (-5) 5) : [C (-6) 5]
generateBlock fallingList rands =
  fallingList !! ((fromIntegral $ head $ rands) `mod` (length fallingList))

--rotates a block
--True = left, False = right
rotateBlock :: Bool -> State -> State
rotateBlock isLeft (S time fallingList falling static rands score) =
  let rotatedList = rotate falling in--leftCheck $ rightCheck $ downCheck $ rotate falling in
  if hitWall rotatedList then S time fallingList falling static rands score else
  S time fallingList rotatedList static rands score where
    hitWall lst = foldr (\(C x _) acc -> (x <= -8 || x >= 8) || acc) False lst
    --this rotates the block
    rotate :: [Coord] -> [Coord]
    rotate lst = map (\(C x y) -> case isLeft of
      True  -> (C (topY - y + leftX) (blockWidth + x - leftX - 1 + topY))
      False -> (C (blockHeight + y - topY - 1 + leftX) (leftX - x + topY))
      ) lst
    leftX = getMin L falling
    topY = getMin U falling
    blockHeight = getMin U falling - getMin D falling
    blockWidth = getMin R falling - getMin L falling
    --this shifts the block up/left/right in case of overlap
    leftCheck lst = shiftBlock (getDMost R falling static - getMin L lst) R lst
    rightCheck lst = shiftBlock (getDMost L falling static - getMin R lst) L lst
    downCheck lst = shiftBlock (getDMost U falling static - getMin D lst) U lst
    --these functions determine how much to do the shifting
    getDMost d l1 l2 = foldr (\(C x y) dMost ->
      case d of
        L -> if x >= 8 then min dMost x else dMost
        R -> if x <= -8 then max dMost x else dMost
        D -> if elem (C x y) l2 then min dMost y else dMost
        U -> if elem (C x y) l2 then max dMost y else dMost
      ) (getFirstEl d l1) l1

--gets the left-most/right-most/top/bottom part of a block
getMin dir l = foldr (
    \(C x y) currMin -> case dir of
    L -> min x currMin
    R -> max x currMin
    D -> min y currMin
    U -> max y currMin
  ) (getFirstEl dir l) l

--gets the first coord from a list of coords (either x or y, depending on direction)
getFirstEl dir ((C x y):_) = case dir of
  L -> x
  R -> x
  U -> y
  D -> y


--drawing
wall, box :: Picture
box = colored brown (solidRectangle 1 1)
wall =    colored (grey 0.4) (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Box     = box
drawTile Blank   = blank
drawTile Wall    = wall

pictureOfBoard :: [Coord] -> [Coord] -> Integer -> Picture
pictureOfBoard falling static score = atCoord (C (-10) 0) (text (pack $ show score)) & draw21times (\r -> draw21times (\c -> drawTileAt (board falling static (C r c)) (C r c)))
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
