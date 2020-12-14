import SplitOn
import Data.List
import Data.Maybe

main = do
    contents <- readFile "day13_input"
    let idS:scheduleS:_ = lines contents
        id = read idS ::Integer
        schedule = (read::String->Integer) <$> filter (/= "x") (splitOn scheduleS ',')
        res = fst $ fromJust $ find (\x -> snd x == 0) $ modWithResults <$> [id..] <*> schedule
    print $ (fst res - id) * snd res
    -- Part 2
    let ids = filter (\x -> snd x /= "x") $ zip [0..] (splitOn scheduleS ',')
        schedule = map (\(a,b) -> let br = read b::Integer in (if a==0 then 0 else br - (a `mod` br), br)) ids 
        res = foldl1 solveCongruence schedule
    print res

solveCongruence::(Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
solveCongruence (r,a) (s,b) =
  let lcm = a * b
      ma = b * ((inverse (b`mod`a) a*r) `mod` a)
      mb = a * ((inverseMod (a`mod`b) b*s) `mod` b)
   in ((ma+mb) `mod` lcm, lcm)

inverseMod::Integer -> Integer -> Integer
inverseMod i m = (i^(m-2)) `mod` m

-- from https://gist.github.com/lovasoa/0e52bcbc937f3d26224f303669ca2b0f
inverse b a =
  let
    next a b = zipWith (-) a $ map (*(head $ zipWith div a b)) b
    l = [a,0] : [b,1] : zipWith next l (tail l)
    r = head $ tail $ head $ filter ((==1).head) l
  in
    if r > 0 then r else a + r
      


modWithResults::Integer -> Integer -> ((Integer, Integer), Integer)
modWithResults a b = ((a,b), a `mod` b)
