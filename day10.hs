import Data.List
import qualified Data.Map as Map

main = do
    contents <- readFile "day10_input"
    let input = (read::String->Int) <$> lines contents
    -- Part 1
        sortedInput = 0:sort input ++ [maximum input + 3]
        diffs =  zipWith (-) (tail sortedInput) sortedInput
        [ones,threes] = group $ sort diffs
        res = length ones * length threes
    print res
    -- Part 2
    let m = Map.fromList (zip (tail sortedInput) (repeat 0))
        res = countWays 0 m (tail sortedInput)
    print res

countWays:: Int -> Map.Map Int Int -> [Int] -> Int
countWays _ m [] = last (Map.elems m)
countWays cur m input =
  let opts = takeWhile (\x -> x-cur <=3) input
      curWays =
        case (Map.lookup cur m) of (Just i) -> i
                                   Nothing -> 1
      newMap = foldl (\acc c -> updateCount c acc curWays) m opts
  in countWays (head input) newMap (tail input)

updateCount::Int -> Map.Map Int Int -> Int -> Map.Map Int Int
updateCount key m val =
  case Map.lookup key m of (Just i) -> Map.insert key (i + val) m
                           Nothing -> Map.insert key val m
