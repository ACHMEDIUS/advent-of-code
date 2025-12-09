import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Ord (comparing)

type Point = (Int, Int, Int)

-- Split string by delimiter
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where rest = splitOn delim cs

-- Parse a line like "162,817,812" into (162, 817, 812)
parsePoint :: String -> Point
parsePoint s = (read x, read y, read z)
  where [x, y, z] = splitOn ',' s

-- Euclidean distance between two 3D points
distance :: Point -> Point -> Double
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ dx*dx + dy*dy + dz*dz
  where
    dx = fromIntegral (x1 - x2)
    dy = fromIntegral (y1 - y2)
    dz = fromIntegral (z1 - z2)

-- Union-Find using a Map
-- parent map: index -> parent index
type Parent = Map.Map Int Int

-- Find root with path compression (returns new map and root)
find :: Parent -> Int -> (Parent, Int)
find parent x =
  case Map.lookup x parent of
    Nothing -> (Map.insert x x parent, x)
    Just p | p == x -> (parent, x)
           | otherwise ->
               let (parent', root) = find parent p
               in (Map.insert x root parent', root)

-- Union two elements
union :: Parent -> Int -> Int -> Parent
union parent x y =
  let (parent1, rootX) = find parent x
      (parent2, rootY) = find parent1 y
  in if rootX == rootY
     then parent2
     else Map.insert rootX rootY parent2

-- Apply n unions from sorted pairs
applyUnions :: Parent -> [(Int, Int, Double)] -> Int -> Parent
applyUnions parent _ 0 = parent
applyUnions parent [] _ = parent
applyUnions parent ((i, j, _):rest) n = applyUnions (union parent i j) rest (n - 1)

-- Get all circuit sizes
getCircuitSizes :: Parent -> Int -> [Int]
getCircuitSizes parent n = Map.elems counts
  where
    -- Find root for each node
    (finalParent, roots) = foldr (\i (p, rs) ->
        let (p', r) = find p i
        in (p', r:rs)) (parent, []) [0..n-1]
    -- Count nodes per root
    counts = foldr (\r m -> Map.insertWith (+) r 1 m) Map.empty roots

main :: IO ()
main = do
  content <- readFile "input.txt"
  let points = map parsePoint $ lines content
      n = length points

      -- Generate all pairs with distances
      pairs = [(i, j, distance (points !! i) (points !! j))
              | i <- [0..n-1], j <- [i+1..n-1]]

      -- Sort by distance
      sortedPairs = sortBy (comparing (\(_,_,d) -> d)) pairs

      -- Apply 1000 unions
      parent = applyUnions Map.empty sortedPairs 1000

      -- Get sizes and sort descending
      sizes = reverse $ sortBy compare $ getCircuitSizes parent n

      -- Multiply top 3
      result = (sizes !! 0) * (sizes !! 1) * (sizes !! 2)

  print result
