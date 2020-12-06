import Data.List

main = do
    contents <- readFile "day6_input"
    let input = lines contents
    -- Part 1
        res = sum $ map (length . nub) $ getGroups input
    print res
    -- Part 2
    let res = sum $ map length $ getGroups2 input
    print res

getGroups::[String] -> [String]
getGroups [] = []
getGroups s =
  let (g, rest) = break (== "") s
   in [concat g] ++ getGroups (dropWhile (=="") rest)

getGroups2::[String] -> [String]
getGroups2 [] = []
getGroups2 s =
  let (g, rest) = break (== "") s
      all = foldl intersect (head g) g
   in [all]  ++ getGroups2 (dropWhile (=="") rest)
