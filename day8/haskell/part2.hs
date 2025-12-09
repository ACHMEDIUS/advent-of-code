import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

type Point = (Int, Int, Int)

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where rest = splitOn delim cs

parsePoint :: String -> Point
parsePoint s = (read x, read y, read z)
  where [x, y, z] = splitOn ',' s

distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ dx*dx + dy*dy + dz*dz
  where
    dx = fromIntegral (x1 - x2)
    dy = fromIntegral (y1 - y2)
    dz = fromIntegral (z1 - z2)

type Parent = Map.Map Int Int

find :: Parent -> Int -> (Parent, Int)
find parent x =
  case Map.lookup x parent of
    Nothing -> (Map.insert x x parent, x)
    Just p | p == x -> (parent, x)
           | otherwise ->
               let (parent', root) = find parent p
               in (Map.insert x root parent', root)

-- Union returns (newParent, didMerge)
union :: Parent -> Int -> Int -> (Parent, Bool)
union parent x y =
  let (parent1, rootX) = find parent x
      (parent2, rootY) = find parent1 y
  in if rootX == rootY
     then (parent2, False)
     else (Map.insert rootX rootY parent2, True)

-- Find last connection that completes the circuit
findLastConnection :: Parent -> [(Int, Int, Double)] -> Int -> (Int, Int)
findLastConnection _ [] _ = error "No connection found"
findLastConnection parent ((i, j, _):rest) needed =
  let (newParent, merged) = union parent i j
  in if merged
     then if needed == 1
          then (i, j)  -- This was the last needed merge
          else findLastConnection newParent rest (needed - 1)
     else findLastConnection newParent rest needed

main :: IO ()
main = do
  content <- readFile "input.txt"
  let points = map parsePoint $ lines content
      n = length points

      pairs = [(i, j, distance (points !! i) (points !! j))
              | i <- [0..n-1], j <- [i+1..n-1]]

      sortedPairs = sortBy (comparing (\(_,_,d) -> d)) pairs

      -- Need n-1 merges to connect n nodes
      (lastI, lastJ) = findLastConnection Map.empty sortedPairs (n - 1)

      -- Get X coordinates and multiply
      (x1, _, _) = points !! lastI
      (x2, _, _) = points !! lastJ
      result = x1 * x2

  print result
