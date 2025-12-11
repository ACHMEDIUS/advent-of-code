import System.Environment (getArgs)
import Data.Bits (xor, testBit, popCount, shiftL)

-- Split string by delimiter
splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim s = case break (== delim) s of
  (a, []) -> [a]
  (a, _:rest) -> a : splitOn delim rest

-- Extract all parenthesized groups from string
extractParens :: String -> [String]
extractParens [] = []
extractParens s = case dropWhile (/= '(') s of
  [] -> []
  (_:rest) -> takeWhile (/= ')') rest : extractParens (dropWhile (/= ')') rest)

-- Parse a line and return minimum presses
parseLine :: String -> Int
parseLine line = minPresses target buttons
  where
    -- Extract diagram between [ and ]
    diagram = takeWhile (/= ']') $ drop 1 $ dropWhile (/= '[') line
    numLights = length diagram
    target = sum [if c == '#' then 1 `shiftL` i else 0 | (i, c) <- zip [0..] diagram]

    -- Extract and parse buttons
    buttonStrs = extractParens line
    parseButton str = sum [1 `shiftL` idx | idxStr <- splitOn ',' str,
                                            let idx = read idxStr :: Int,
                                            idx < numLights]
    buttons = map parseButton buttonStrs

-- Find minimum presses for a machine
minPresses :: Int -> [Int] -> Int
minPresses target buttons = minimum validPresses
  where
    numButtons = length buttons
    allCombos = [0..2^numButtons - 1] :: [Int]

    compute combo = foldl xor 0 [buttons !! b | b <- [0..numButtons-1], testBit combo b]

    validPresses = [popCount combo | combo <- allCombos, compute combo == target]

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "input.txt" else head args
  content <- readFile filename
  let total = sum $ map parseLine $ lines content
  print total
