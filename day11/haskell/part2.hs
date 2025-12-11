import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.Bits (setBit, testBit)

parseLine :: String -> (String, [String])
parseLine line = (node, outputs)
  where
    (node, rest) = break (== ':') line
    outputs = words $ drop 2 rest

-- State key: (node, mask) where mask tracks visited required nodes
type MemoKey = (String, Int)
type Memo = Map.Map MemoKey Int

-- Update mask based on node
updateMask :: String -> Int -> Int
updateMask "dac" m = setBit m 0
updateMask "fft" m = setBit m 1
updateMask _ m = m

countPaths :: Map.Map String [String] -> String -> Int -> Memo -> (Int, Memo)
countPaths graph node mask memo =
  let mask' = updateMask node mask
  in if node == "out"
     then (if mask' == 3 then 1 else 0, memo)
     else case Map.lookup (node, mask') memo of
       Just val -> (val, memo)
       Nothing ->
         let outputs = Map.findWithDefault [] node graph
             (total, memo') = foldl addPath (0, memo) outputs
         in (total, Map.insert (node, mask') total memo')
  where
    mask' = updateMask node mask
    addPath (acc, m) next =
      let (count, m') = countPaths graph next mask' m
      in (acc + count, m')

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "input.txt" else head args
  content <- readFile filename
  let graph = Map.fromList $ map parseLine $ lines content
      (result, _) = countPaths graph "svr" 0 Map.empty
  print result
