{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.List

data Coord = C Integer Integer

data Direction = R | U | L | D deriving Eq

data Tile = Box | Blank deriving Eq

box :: Picture
box = colored brown (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Box    = box
drawTile Blank  = blank

block :: [Coord]
block = []

main = drawingOf (drawTile Box)
