import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let rotations = lines content
        result = countAllZeros 50 rotations
    print result

countAllZeros :: Int -> [String] -> Int
countAllZeros _ [] = 0
countAllZeros pos (rot:rots) = crosses + countAllZeros newPos rots
  where
    dir = head rot
    dist = read (tail rot) :: Int

    crosses = if dir == 'L'
              then countZerosL pos dist
              else countZerosR pos dist

    newPos = ((pos + if dir == 'L' then -dist else dist) `mod` 100 + 100) `mod` 100

countZerosL :: Int -> Int -> Int
countZerosL pos dist
    | pos == 0 = dist `div` 100
    | dist >= pos = (dist - pos) `div` 100 + 1
    | otherwise = 0

countZerosR :: Int -> Int -> Int
countZerosR pos dist = (pos + dist) `div` 100
