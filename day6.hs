import Data.List

main = do
    contents <- readFile "day6_input"
    let input = lines contents
    -- Part 1
        res = sum $ length . nub <$> getGroups input concat
    print res
    -- Part 2
    let res = sum $ length <$> getGroups input (\x -> foldl intersect (head x) x)
    print res

getGroups::[String] -> ([String] -> String) -> [String]
getGroups [] _ = []
getGroups s f =
  let (g, rest) = break (== "") s
   in [f g] ++ getGroups (dropWhile (== "") rest) f
