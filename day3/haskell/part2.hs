import System.Environment (getArgs)
import Data.Char (digitToInt)
import Data.List (sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let total = sum (map maxJoltage12 (lines content))
    print total

maxJoltage12 :: String -> Integer
maxJoltage12 line = read result
  where
    n = length line
    k = 12
    result = select k (-1) ""

    select 0 _ acc = acc
    select remaining lastPos acc =
        let searchEnd = n - k + (k - remaining)
            (bestDigit, bestPos) = maximum [(line !! j, j) | j <- [lastPos + 1 .. searchEnd]]
        in select (remaining - 1) bestPos (acc ++ [bestDigit])
