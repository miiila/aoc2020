main = do
    contents <- readFile "day2_input"
    let input = lines contents
    -- Part 1
    let res = length $ filter (isValid . getEntry) input
    print res
    -- Part 2
    let res = length $ filter (isValidSecond . getEntry) input
    print res

isValid::(Int, Int, Char, String) -> Bool
isValid (low, high, char, str) =
    low <= count && count <= high
        where count = length $ filter (== char) str

isValidSecond::(Int, Int, Char, String) -> Bool
isValidSecond (low, high, char, str) =
    left /= right
        where left = str !! (low - 1)  == char
              right = str !! (high - 1) == char


getEntry::String -> (Int, Int, Char, String)
getEntry s =
    let (min, _:rest) = break (=='-') s
        (max, _:c:_:_:r) = break (==' ') rest
     in ((read::String->Int) min, (read::String->Int) max, c, r)
