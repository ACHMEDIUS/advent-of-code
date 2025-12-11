import System.Environment (getArgs)

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where rest = splitOn delim cs

parsePoint :: String -> (Int, Int)
parsePoint s = (read x, read y)
  where [x, y] = splitOn ',' s

rectangleArea :: (Int, Int) -> (Int, Int) -> Int
rectangleArea (x1, y1) (x2, y2) = width * height
  where
    width = abs (x2 - x1) + 1
    height = abs (y2 - y1) + 1

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "input.txt" else head args
  content <- readFile filename
  let tiles = map parsePoint $ lines content

      -- All pairs of tiles
      pairs = [(tiles !! i, tiles !! j)
              | i <- [0..length tiles - 1]
              , j <- [i+1..length tiles - 1]]

      -- Calculate area for each pair
      areas = map (uncurry rectangleArea) pairs

      maxArea = maximum areas

  print maxArea
