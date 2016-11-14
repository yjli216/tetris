{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.List 

main :: IO ()
main = interactionOf ([], []) handleTime handleEvent drawState
-- main = drawingOf (pictureOfBoard board)

handleTime :: Double -> ([Coord], [Coord]) -> ([Coord], [Coord]) 
handleTime _ c = c

handleEvent :: Event -> ([Coord], [Coord]) -> ([Coord], [Coord]) 
handleEvent _ c = c

drawState :: ([Coord], [Coord]) -> Picture
drawState (falling, static) = pictureOfBoard falling static

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
          | abs x == 10 || abs y == 10 = Wall
          | otherwise          = Blank

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Tile -> Coord -> Picture
drawTileAt tile c = atCoord c (drawTile tile)

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic



