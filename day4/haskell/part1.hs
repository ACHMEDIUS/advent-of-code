import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let grid = lines content
        result = countAccessible grid
    print result

countAccessible :: [String] -> Int
countAccessible grid = sum [1 | r <- [0..rows-1],
                                 c <- [0..cols-1],
                                 grid !! r !! c == '@',
                                 countNeighbors grid r c < 4]
  where
    rows = length grid
    cols = length (head grid)

countNeighbors :: [String] -> Int -> Int -> Int
countNeighbors grid r c = length (filter isRoll neighbors)
  where
    rows = length grid
    cols = length (head grid)

    -- 8 directions
    directions = [(-1,-1), (-1,0), (-1,1),
                  (0,-1),          (0,1),
                  (1,-1),  (1,0),  (1,1)]

    neighbors = [(r + dr, c + dc) | (dr, dc) <- directions]

    isRoll (nr, nc) = nr >= 0 && nr < rows &&
                      nc >= 0 && nc < cols &&
                      grid !! nr !! nc == '@'
