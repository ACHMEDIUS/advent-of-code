import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let grid = lines content
        result = removeAll grid
    print result

removeAll :: [String] -> Int
removeAll grid = go grid 0
  where
    go currentGrid total =
        let accessible = findAccessible currentGrid
        in if null accessible
           then total
           else let newGrid = removeRolls currentGrid accessible
                in go newGrid (total + length accessible)

findAccessible :: [String] -> [(Int, Int)]
findAccessible grid = [(r, c) | r <- [0..rows-1],
                                c <- [0..cols-1],
                                grid !! r !! c == '@',
                                countNeighbors grid r c < 4]
  where
    rows = length grid
    cols = length (head grid)

removeRolls :: [String] -> [(Int, Int)] -> [String]
removeRolls grid positions =
    [[if (r, c) `elem` positions then '.' else grid !! r !! c
      | c <- [0..cols-1]]
     | r <- [0..rows-1]]
  where
    rows = length grid
    cols = length (head grid)

countNeighbors :: [String] -> Int -> Int -> Int
countNeighbors grid r c = length (filter isRoll neighbors)
  where
    rows = length grid
    cols = length (head grid)

    directions = [(-1,-1), (-1,0), (-1,1),
                  (0,-1),          (0,1),
                  (1,-1),  (1,0),  (1,1)]

    neighbors = [(r + dr, c + dc) | (dr, dc) <- directions]

    isRoll (nr, nc) = nr >= 0 && nr < rows &&
                      nc >= 0 && nc < cols &&
                      grid !! nr !! nc == '@'
