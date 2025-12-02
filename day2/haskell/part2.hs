import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let ranges = map parseRange (takeWhile (/= "") (lines content))
        total = sum [n | (start, end) <- ranges, n <- [start..end], isInvalid n]
    print total

parseRange :: String -> (Int, Int)
parseRange line = (read start, read end)
  where
    (start, rest) = span (/= '-') line
    end = tail rest

isInvalid :: Int -> Bool
isInvalid n = any isRepeated [1..len `div` 2]
  where
    s = show n
    len = length s
    isRepeated patLen = len `mod` patLen == 0 &&
                        all (== pattern) (chunks patLen s)
      where
        pattern = take patLen s

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)
