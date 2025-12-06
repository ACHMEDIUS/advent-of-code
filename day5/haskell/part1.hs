import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let allLines = lines content
        (rangesText, rest) = break null allLines
        ingredientsText = filter (not . null) (drop 1 rest)
        ranges = map parseRange rangesText
        ingredients = map read ingredientsText
        freshCount = length (filter (isFresh ranges) ingredients)
    print freshCount

parseRange :: String -> (Int, Int)
parseRange line = (read start, read end)
  where
    (start, rest) = span (/= '-') line
    end = tail rest

isFresh :: [(Int, Int)] -> Int -> Bool
isFresh ranges id = any inRange ranges
  where
    inRange (start, end) = id >= start && id <= end
