import System.Environment (getArgs)
import Data.List (transpose)
import Data.Char (isSpace)

main :: IO ()
main = do
    args <- getArgs
    let filename = if null args then "input.txt" else head args
    content <- readFile filename
    let linesText = lines content
        padded = padLines linesText
        columns = transpose padded
        problems = groupProblems columns
        results = map solveProblem problems
        grandTotal = sum results
    print grandTotal

padLines :: [String] -> [String]
padLines ls = map padTo ls
  where
    maxLen = maximum (map length ls)
    padTo s = s ++ replicate (maxLen - length s) ' '

groupProblems :: [String] -> [[String]]
groupProblems = go []
  where
    go acc [] = if null acc then [] else [reverse acc]
    go acc (col:cols)
        | all isSpace col = if null acc
                            then go [] cols
                            else reverse acc : go [] cols
        | otherwise = go (col:acc) cols

solveProblem :: [String] -> Int
solveProblem cols = result
  where
    -- Reverse columns for right-to-left reading
    reversedCols = reverse cols

    -- For each column, read top-to-bottom to form a number
    numbers = map readColumn reversedCols

    -- Operator from last row of any column
    operator = head (filter (not . isSpace) (map last cols))

    result = case operator of
               '*' -> product numbers
               '+' -> sum numbers

readColumn :: String -> Int
readColumn col = read numStr
  where
    -- Read all but last char (operator row), filter out spaces
    numStr = filter (not . isSpace) (init col)
