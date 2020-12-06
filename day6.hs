import SplitOn
import Data.List

main = do
    contents <- readFile "day6_input"
    let input = splitOn (lines contents) ""
    -- Part 1
        res = sum $ length . nub . concat <$> input
    print res
    -- Part 2
    let res = sum $ length . foldl1 intersect <$> input
    print res
