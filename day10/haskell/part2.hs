import System.Environment (getArgs)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn delim s = case break (== delim) s of
  (a, []) -> [a]
  (a, _:rest) -> a : splitOn delim rest

extractParens :: String -> [String]
extractParens [] = []
extractParens s = case dropWhile (/= '(') s of
  [] -> []
  (_:rest) -> takeWhile (/= ')') rest : extractParens (dropWhile (/= ')') rest)

extractCurly :: String -> String
extractCurly s = takeWhile (/= '}') $ drop 1 $ dropWhile (/= '{') s

-- Simple brute force for small instances, Gaussian elimination for larger
solve :: [[Int]] -> [Int] -> Int
solve buttons targets
  | m <= 6 = bruteForce buttons targets
  | otherwise = gaussianSolve buttons targets
  where m = length buttons

bruteForce :: [[Int]] -> [Int] -> Int
bruteForce buttons targets = minimum [s | s <- allSums, s >= 0]
  where
    n = length targets
    m = length buttons
    maxT = maximum targets

    allSums = map tryCombo allCombos

    allCombos = sequence [[0..maxT] | _ <- [1..m]]

    tryCombo presses =
      let counters = [sum [presses !! j | j <- [0..m-1], i `elem` (buttons !! j)] | i <- [0..n-1]]
      in if counters == targets then sum presses else maxT * m + 1

gaussianSolve :: [[Int]] -> [Int] -> Int
gaussianSolve buttons targets = best
  where
    n = length targets
    m = length buttons
    maxT = maximum targets

    -- Build augmented matrix
    matrix0 = [[if j < m then if i `elem` (buttons !! j) then 1 else 0 else targets !! i
               | j <- [0..m]] | i <- [0..n-1]]

    -- Gaussian elimination
    (matrix, pivots) = gaussElim matrix0 0 0 []

    gaussElim mat row col piv
      | col >= m || row >= n = (mat, piv)
      | otherwise = case findPivotRow mat row col of
          Nothing -> gaussElim mat row (col+1) piv
          Just pr ->
            let mat' = swapRows mat row pr
                mat'' = eliminate mat' row col
            in gaussElim mat'' (row+1) (col+1) ((col, row):piv)

    findPivotRow mat r c =
      case filter (\i -> (mat !! i) !! c /= 0) [r..n-1] of
        [] -> Nothing
        (i:_) -> Just i

    swapRows mat r1 r2
      | r1 == r2 = mat
      | otherwise = [if i == r1 then mat !! r2 else if i == r2 then mat !! r1 else mat !! i | i <- [0..n-1]]

    eliminate mat r c = [if i == r then row else elimRow row (mat !! r) c | (i, row) <- zip [0..] mat]

    elimRow row pivRow c
      | row !! c == 0 = row
      | otherwise = [row !! j * pv - sv * pivRow !! j | j <- [0..m]]
      where pv = pivRow !! c; sv = row !! c

    -- Free variables
    pivotCols = map fst pivots
    freeVars = [j | j <- [0..m-1], j `notElem` pivotCols]
    numFree = length freeVars

    -- Search over free variables
    best = if numFree > 4
           then searchLimited
           else searchAll

    searchAll = minimum $ map evalFreeVals allFreeVals
      where
        allFreeVals = sequence [[0..maxT] | _ <- [1..numFree]]

    searchLimited = minimum $ map evalFreeVals limitedFreeVals
      where
        limitedFreeVals = sequence [[0..min 20 maxT] | _ <- [1..numFree]]

    evalFreeVals freeVals =
      case backSub freeVals of
        Nothing -> maxT * m + 1
        Just sol -> sum sol

    backSub freeVals = go (sortBy (comparing (negate . snd)) pivots) sol0
      where
        sol0 = [if j `elem` freeVars then freeVals !! indexOf j freeVars else 0 | j <- [0..m-1]]
        indexOf x xs = length $ takeWhile (/= x) xs

        go [] sol = Just sol
        go ((col, row):rest) sol =
          let r = matrix !! row
              pv = r !! col
              rhs = (r !! m) - sum [r !! j * sol !! j | j <- [0..m-1], j /= col]
          in if pv == 0 || rhs `mod` pv /= 0 then Nothing
             else let v = rhs `div` pv
                  in if v < 0 then Nothing
                     else go rest (take col sol ++ [v] ++ drop (col+1) sol)

parseLine :: String -> Int
parseLine line = solve buttons targets
  where
    buttonStrs = extractParens line
    parseButton str = map (\s -> read s :: Int) $ splitOn ',' str
    buttons = map parseButton buttonStrs
    targetStr = extractCurly line
    targets = map (\s -> read s :: Int) $ splitOn ',' targetStr

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "input.txt" else head args
  content <- readFile filename
  let total = sum $ map parseLine $ lines content
  print total
