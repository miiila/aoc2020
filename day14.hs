import SplitOn
import qualified Data.Map as Map

main = do
    contents <- readFile "day14_input"
    let instructions = readIns <$> lines contents
    -- Part 1
        memory = Map.fromList [(0,0)] 
        res = foldl (\(accMask, accMem) c -> handleIns c accMask accMem) ("",memory) instructions
    print $ sum $ filter (>0) $ Map.elems $ snd res
    -- Part 2
    let memory = Map.fromList [(0,0)] 
        res = foldl (\(accMask, accMem) c -> handleInsPart2 c accMask accMem) ("",memory) instructions
    print $ sum $ filter (>0) $ Map.elems $ snd res


handleInsPart2::Instruction -> String -> Memory -> (String, Memory)
handleInsPart2 (Mask m) _ mem = (m, mem)
handleInsPart2 (Ptr (add, val)) mask mem = 
  let addrs = fromBinString <$> ((`applyMaskPart2` mask) . toBinString) add
      finalMem = foldl (\acc c -> Map.insert c val acc) mem addrs 
   in (mask, finalMem)

handleIns::Instruction -> String -> Memory -> (String, Memory)
handleIns (Mask m) _ mem = (m, mem)
handleIns (Ptr (add, val)) mask mem = (mask, Map.insert add ((fromBinString . (`applyMask` mask) . toBinString) val) mem)

applyMaskPart2::String->String->[String]
applyMaskPart2 [] _ = [[]]
applyMaskPart2 (x:xs) (y:ys) =
  let next = case y of '0' -> [x]
                       '1' -> [y]
                       'X' -> "01"
   in (:) <$> next <*> applyMaskPart2 xs ys

applyMask::String->String->String
applyMask num mask = zipWith (\n m -> if m == 'X' then n else m) num mask

toBinString::Int->String
toBinString i = 
  let bin = toBinStringRec i ""
   in replicate (36 - length bin) '0' ++ bin

toBinStringRec::Int->String->String
toBinStringRec 0 s = s
toBinStringRec i s = toBinStringRec (i `div` 2) (show (i `mod` 2))++s

fromBinString::String->Int
fromBinString s = sum $ zipWith (*) powTwo ((read::String->Int) . (: []) <$> reverse s)

powTwo = [2^x | x <- [0..]]

readIns::String -> Instruction
readIns s 
  | take 2 s == "ma" = Mask (last $ words s)
  | otherwise = Ptr (read $ drop 4 $ head $ splitOn s ']', read $ last $ words s) 

data Instruction = Mask String | Ptr (Int, Int) deriving (Show)
type Memory = Map.Map Int Int
