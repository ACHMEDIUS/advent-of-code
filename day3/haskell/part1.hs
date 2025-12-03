import System.Environment (getArgs)
import Data.Char (digitToInt)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let total = sum (map maxJoltage (lines content))
    print total

maxJoltage :: String -> Int
maxJoltage line = maximum [digitToInt (line !! i) * 10 + maxAfter !! i | i <- [0..n-2]]
  where
    n = length line
    maxAfter = scanr max 0 (map digitToInt line)
