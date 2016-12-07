import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Tetris
import CodeWorld

indexList :: [a] -> [(a, Integer)]
indexList l = indices 0 l where
  indices _ [] = []
  indices i (hd:tl) = (hd, i) : indices (i+1) tl

parseBlock :: [String] -> [Coord]
parseBlock lst = concat $ map strToCoords (indexList lst) where
  strToCoords :: (String, Integer) -> [Coord]
  strToCoords (line, y) = catMaybes $ map (getCoord y) (indexList line)
  getCoord :: Integer -> (Char, Integer) -> Maybe Coord
  getCoord y (char, x) = if char == '#' then Just (C x y) else Nothing

parseFile :: String -> Either String [[Coord]]
parseFile file =
  let blocks = splitOn "\n\n" file in
  let blockList = map (\block -> parseBlock $ lines block) blocks in
  if findError blockList then Left "error" else Right blockList where

findError :: [[Coord]] -> Bool
findError list = go 4 list where
  go 0 []      = True
  go 0 _       = False
  go n []      = False
  go n (hd:tl) = isValidCoords hd && go (n-1) tl
  isValidCoords coords = (length coords <= 16) && (and $ map (\c -> validCoord c) coords)
  validCoord (C x y) = x >= 0 && x <= 3 && y >= 0 && y <= 3

main :: IO ()
main = do
  filepath <- head <$> getArgs
  file <- readFile filepath
  case parseFile file of
    Left _ -> putStrLn "input file is not properly formatted!"
    Right blocks -> runInteraction $ resetable $ Interaction initializeState handleTime handleEvent drawState where
      initializeState = mkInitialState initializeBlocks
      initializeBlocks :: [[Coord]]
      initializeBlocks = moveUp 15 blocks --moves all blocks up by 5 units to the top of the screen
      moveUp :: Integer -> [[Coord]] -> [[Coord]]
      moveUp 0 lst = lst
      moveUp n lst = moveUp (n-1) (map (\block -> map (move U) block) lst)