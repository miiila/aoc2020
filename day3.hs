main = do
    contents <- readFile "day3_input"
    let input = map (concat . repeat) (lines contents)
    -- Part 1
    let res =  countTrees 0 0 ((+1), (+3)) input 0
    print res
    -- Part 2
    let slopes = [((+1), (+1)), ((+1), (+3)), ((+1), (+5)), ((+1), (+7)), ((+2), (+1))]
        appliedSlopes = countTrees 0 0 <$> slopes
        res = product $ appliedSlopes <*> [input] <*> [0]
    print res


countTrees::Int -> Int -> (Int -> Int, Int -> Int) -> [String] -> Int -> Int
countTrees row col (rowAdd, colAdd) input trees
  | row >= length input = trees
  | otherwise = countTrees (rowAdd row) (colAdd col) (rowAdd, colAdd) input (if (input !! row) !! col == '#' then trees+1 else trees)
