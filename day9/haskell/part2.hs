import System.Environment (getArgs)
import qualified Data.Set as Set
import Data.List (foldl')

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where rest = splitOn delim cs

parsePoint :: String -> (Int, Int)
parsePoint s = (read x, read y)
  where [x, y] = splitOn ',' s

-- Draw line between two points (horizontal or vertical)
drawLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
drawLine (x1, y1) (x2, y2)
  | x1 == x2  = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | otherwise = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]

-- Flood fill to find exterior cells
floodFill :: Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
floodFill boundary (minX, minY) (maxX, maxY) start = go Set.empty [start]
  where
    go visited [] = visited
    go visited (p@(x,y):queue)
      | x < minX || x > maxX || y < minY || y > maxY = go visited queue
      | Set.member p visited = go visited queue
      | Set.member p boundary = go visited queue
      | otherwise = go (Set.insert p visited) (queue ++ neighbors)
      where neighbors = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]

-- Check if rectangle is fully inside (not exterior)
isValidRect :: Set.Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isValidRect exterior (x1, y1) (x2, y2) =
  all (\p -> not (Set.member p exterior)) allPoints
  where
    allPoints = [(x, y) | x <- [min x1 x2 .. max x1 x2], y <- [min y1 y2 .. max y1 y2]]

rectangleArea :: (Int, Int) -> (Int, Int) -> Int
rectangleArea (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "input.txt" else head args
  content <- readFile filename
  let redTiles = map parsePoint $ lines content
      n = length redTiles

      -- Draw boundary lines between consecutive red tiles
      boundaryLines = concat [drawLine (redTiles !! i) (redTiles !! ((i+1) `mod` n))
                             | i <- [0..n-1]]
      boundary = Set.fromList boundaryLines

      -- Find bounding box with padding
      xs = map fst redTiles
      ys = map snd redTiles
      minX = minimum xs - 1
      maxX = maximum xs + 1
      minY = minimum ys - 1
      maxY = maximum ys + 1

      -- Flood fill from corner to find exterior
      exterior = floodFill boundary (minX, minY) (maxX, maxY) (minX, minY)

      -- Find valid rectangles
      pairs = [(redTiles !! i, redTiles !! j)
              | i <- [0..n-1], j <- [i+1..n-1]]

      validAreas = [rectangleArea p1 p2
                   | (p1, p2) <- pairs
                   , isValidRect exterior p1 p2]

      maxArea = maximum validAreas

  print maxArea
