import System.Environment (getArgs)
import qualified Data.Set as Set

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let grid = lines content
        cols = length (head grid)
        startCol = head [c | c <- [0..cols-1], grid !! 0 !! c == 'S']
        result = simulate grid (Set.singleton (0, startCol)) Set.empty
    print (Set.size result)

simulate :: [String] -> Set.Set (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
simulate grid beams hitSplitters
    | Set.null beams = hitSplitters
    | otherwise = simulate grid newBeams newHitSplitters
  where
    results = map (moveDown grid hitSplitters) (Set.toList beams)
    newHits = Set.fromList [pos | (Just pos, _) <- results]
    newHitSplitters = Set.union hitSplitters newHits
    newBeams = Set.fromList (concatMap snd results)

moveDown :: [String] -> Set.Set (Int, Int) -> (Int, Int) -> (Maybe (Int, Int), [(Int, Int)])
moveDown grid hitSplitters (r, c) = go (r + 1)
  where
    rows = length grid
    cols = length (head grid)

    go row
        | row >= rows = (Nothing, [])
        | grid !! row !! c == '^' =
            if Set.member (row, c) hitSplitters
            then (Nothing, [])
            else (Just (row, c), leftRight)
        | otherwise = go (row + 1)
      where
        leftRight = [(row, c - 1) | c > 0] ++ [(row, c + 1) | c < cols - 1]
