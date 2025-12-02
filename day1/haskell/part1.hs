import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let rotations = lines content
        result = countZeros 50 rotations
    print result

countZeros :: Int -> [String] -> Int
countZeros _ [] = 0
countZeros pos (rot:rots) = (if newPos == 0 then 1 else 0) + countZeros newPos rots
  where
    dir = head rot
    dist = read (tail rot) :: Int
    newPos = if dir == 'L'
             then ((pos - dist) `mod` 100 + 100) `mod` 100
             else (pos + dist) `mod` 100
