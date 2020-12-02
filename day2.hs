import System.IO

main = do
    contents <- readFile "day2_input"
    let input = lines contents
    -- Part 1
    let res = length $ filter (== True) $  map (isValid . getEntry) input
    print res
    -- Part 2
    let res = length $ filter (== True) $  map (isValidSecond . getEntry) input
    print res

isValid::(Int, Int, Char, String) -> Bool
isValid (low, high, char, str) =
    low <= count && count <= high
        where count = foldl (\acc x -> if x == char then acc+1 else acc) 0 str

isValidSecond::(Int, Int, Char, String) -> Bool
isValidSecond (low, high, char, str) =
    left /= right
        where left = head (drop (low - 1) str) == char
              right = head (drop (high - 1) str) == char


getEntry::String -> (Int, Int, Char, String)
getEntry s =
    let (min, _:rest) = break (=='-') s
        (max, _:c:_:_:r) = break (==' ') rest

     in ((read::String->Int) min, (read::String->Int) max, c, r)
