import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (foldl')

parseLine :: String -> (String, [String])
parseLine line = (node, outputs)
  where
    (node, rest) = break (== ':') line
    outputs = words $ drop 2 rest

-- Count paths using memoization
countPaths :: Map.Map String [String] -> String -> Map.Map String Int -> (Int, Map.Map String Int)
countPaths _ "out" memo = (1, memo)
countPaths graph node memo =
  case Map.lookup node memo of
    Just val -> (val, memo)
    Nothing ->
      let outputs = Map.findWithDefault [] node graph
          (total, memo') = foldl' addPath (0, memo) outputs
      in (total, Map.insert node total memo')
  where
    addPath (acc, m) next =
      let (count, m') = countPaths graph next m
      in (acc + count, m')

main :: IO ()
main = do
  args <- getArgs
  let filename = if null args then "input.txt" else head args
  content <- readFile filename
  let graph = Map.fromList $ map parseLine $ lines content
      (result, _) = countPaths graph "you" Map.empty
  print result
