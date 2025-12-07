import System.Environment (getArgs)
import qualified Data.Map as Map

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let grid = lines content
        cols = length (head grid)
        startCol = head [c | c <- [0..cols-1], grid !! 0 !! c == 'S']
        result = simulate grid (Map.singleton (0, startCol) 1) 0
    print result

simulate :: [String] -> Map.Map (Int, Int) Int -> Int -> Int
simulate grid particles exited
    | Map.null particles = exited
    | otherwise = simulate grid newParticles (exited + exitedThisRound)
  where
    results = [moveDown grid (r, c) count | ((r, c), count) <- Map.toList particles]
    newParticlesList = concatMap fst results
    exitedThisRound = sum (map snd results)
    newParticles = Map.fromListWith (+) newParticlesList

moveDown :: [String] -> (Int, Int) -> Int -> ([((Int, Int), Int)], Int)
moveDown grid (r, c) count = go (r + 1)
  where
    rows = length grid
    cols = length (head grid)

    go row
        | row >= rows = ([], count)
        | grid !! row !! c == '^' = (leftRight, 0)
        | otherwise = go (row + 1)
      where
        leftRight = [((row, c - 1), count) | c > 0] ++ [((row, c + 1), count) | c < cols - 1]
