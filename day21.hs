import SplitOn
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
main = do
    contents <- readFile "day21_input"
    --contents <- readFile "day21_input_test"
    let input = lines contents
    -- Part 1
        parsedInput = parseInputLine <$> input
        concatedInput = concat parsedInput
        allrgs = Set.fromList $ fst <$> concatedInput
        ings = concat $ Set.toList . head . map snd <$> parsedInput
        info = Map.fromListWith Set.intersection concatedInput 
    --print parsedInput
        confirmedAllrgs = Set.unions $ Map.elems info
        res = filter (\x -> x `notElem` Set.toList confirmedAllrgs) ings
    print $ length res
    -- Part 2
    let sortedItems = sortBy (\x y -> length (snd x) `compare` length (snd y)) (Map.toList info)
        res = foldl (\(items, used) (key, val) -> ((key, val Set.\\ used):items, val `Set.union` used)) ([],Set.empty) sortedItems 
    print sortedItems
    print $ Map.fromList $ fst res

parseInputLine::String -> [(String, Set.Set String)]
parseInputLine line =
  let [ings, allrgsP] = splitOn line '('
      allrgsS = reverse $ drop 1 $ reverse allrgsP -- remove last )
      allrgs = dropWhile (== ' ') <$> splitOn (dropWhile (/= ' ') allrgsS) ','
      in zip allrgs (repeat $ Set.fromList $ words ings)
