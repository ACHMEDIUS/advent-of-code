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
isInvalid n = even len && firstHalf == secondHalf
  where
    s = show n
    len = length s
    half = len `div` 2
    firstHalf = take half s
    secondHalf = drop half s
