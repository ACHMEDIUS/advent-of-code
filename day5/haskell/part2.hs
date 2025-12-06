import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let rangesText = takeWhile (not . null) (lines content)
        ranges = map parseRange rangesText
        sortedRanges = sortBy (comparing fst) ranges
        merged = mergeRanges sortedRanges
        total = sum [end - start + 1 | (start, end) <- merged]
    print total

parseRange :: String -> (Int, Int)
parseRange line = (read start, read end)
  where
    (start, rest) = span (/= '-') line
    end = tail rest

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges ((s1,e1):rest) = go (s1,e1) rest
  where
    go current [] = [current]
    go (cs, ce) ((s,e):rs)
        | s <= ce + 1 = go (cs, max ce e) rs
        | otherwise = (cs, ce) : go (s, e) rs
