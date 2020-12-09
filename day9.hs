import Data.List

main = do
    contents <- readFile "day9_input"
    let input = (read::String->Int) <$> lines contents
    -- Part 1
        nonValidInput = findNonValid input
    print nonValidInput
    -- Part 2
    let weakness = findNonValidSum input nonValidInput
        res = minimum weakness + maximum weakness
    print res

findNonValidSum::[Int] -> Int -> [Int]
findNonValidSum nums sumToFind =
    let a = head $ dropWhile (\x -> sum x < sumToFind) $ inits nums
     in if sum a == sumToFind then a else findNonValidSum (tail nums) sumToFind

findNonValid::[Int] -> Int
findNonValid nums
 | valid = findNonValid (tail nums)
 | not valid = num
 where (l, num:_) = splitAt 25 nums
       valid = isValid l num

isValid::[Int] -> Int -> Bool
isValid nums num =
    or [x /= y | x <- nums, y <- nums, x+y == num]
