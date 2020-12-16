import SplitOn
import Data.List
import qualified Data.Set as Set

main = do
    contents <- readFile "day16_input"
    --contents <- readFile "day16_input_test"
    let input = lines contents
        inputS = splitOn input ""
        tickets = (\x -> (read::String->Int) <$> splitOn x ',') <$> drop 1 (inputS !! 2)
        ranges = parseFields <$> head inputS
    -- Part 1
        res = sum $ snd <$> concat  ((\x -> filter (not . fst) $ (`validateRanges` concat ranges) <$> x) <$> tickets)
    print res
    -- Part 2
    let valids = (\x -> and $ fst . (`validateRanges` concat ranges) <$> x) <$> tickets
        valid = fst <$> filter snd (zip tickets valids)
        validRanges = transpose valid
        candidates = map fst <$> filter snd <$> map (zip [1..]) (validate ranges <$> validRanges)
        ticket = (read::String->Int) <$> (`splitOn` ',') (inputS !! 1 !! 1)
        ord = head $ filter (\x -> length x == length ticket) $ solve [] (Set.fromList <$> candidates) (Set.fromList [])
        res = product $ fst <$> filter (\x -> 6 >= snd x) (zip ticket ord)
    print res

solve::[Int] -> [Set.Set Int] -> Set.Set Int -> [[Int]]
solve c [] _ = [c]
solve c (a:r) used 
  | null candidates = [c]
  | otherwise = concatMap (\x-> solve (c++[x]) r (Set.insert x used)) (Set.toList candidates)
    where candidates = Set.difference a used 

validate::[[Range]] -> [Int] -> [Bool]
validate  ranges validRanges = (\x -> and $ fst <$> x) <$> chunkify (length validRanges) ((\x -> (`validateRanges` x)) <$> ranges <*> validRanges)

parseFields::String -> [Range]
parseFields s =
  let _:r:_ = splitOn s ':'
      a:_:b:_ = splitOn (drop 1 r) ' '
   in parseRange <$> [a,b]

validateRanges::Int -> [Range] -> (Bool, Int)
validateRanges i r =  (or $ validateRange i <$> r, i)

validateRange::Int -> Range -> Bool
validateRange i r = i >= fst r && i <= snd r

parseRange::String->Range
parseRange s =
  let l:h = read <$> splitOn s '-' 
   in (l, head h)

type Range = (Int,Int)

chunkify::Int -> [t] -> [[t]]
chunkify _ [] = []
chunkify i l = 
  let (h,r) = splitAt i l
   in h:chunkify i r

