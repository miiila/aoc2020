import qualified Data.Map as Map
import Data.List

main = do
    contents <- readFile "day4_input"
    let input = lines contents
        parsedInput = splitInput input
    -- Part 1
        filteredInput = filter isValid $ map toAssoc parsedInput
        res = length filteredInput
    print res
    -- Part 2
    let res = length $ filter (==True) $ map (all isValidEntries . Map.toList) filteredInput
    print res

splitInput::[String] -> [String]
splitInput [] = []
splitInput input =
  let (passport, rest) = break (== "") input
      next = if null rest then [] else tail rest
  in [unwords passport] ++ splitInput next

toAssoc::String -> Map.Map String String
toAssoc s =
  Map.fromList $ map toTuple $ words s

toTuple::String -> (String,String)
toTuple s =
  let (a,_:b) = break (==':') s
  in (a, b)

isValid:: Map.Map String String -> Bool
isValid map =
  all (\x -> Map.member x map) ["ecl", "byr", "iyr", "hcl", "eyr","hgt","pid"]

isValidEntries::(String, String) -> Bool
isValidEntries ("ecl", s) = isValidEcl s
isValidEntries ("byr", s) = isValidByr s
isValidEntries ("iyr", s) = isValidIyr s
isValidEntries ("hcl", s) = isValidHcl s
isValidEntries ("eyr", s) = isValidEyr s
isValidEntries ("hgt", s) = isValidHgt s
isValidEntries ("pid", s) = isValidPid s
isValidEntries ("cid", _) = True

isValidByr::String -> Bool
isValidByr s =
  a >= 1920 && a <= 2002
    where a = (read::String->Int) s

isValidIyr::String -> Bool
isValidIyr s =
  a >= 2010 && a <= 2020
    where a = (read::String->Int) s

isValidEyr::String -> Bool
isValidEyr s =
  a >= 2020 && a <= 2030
    where a = (read::String->Int) s

isValidHgt::String -> Bool
isValidHgt s =
  let (hgt, unit) = splitAt (length s -2) s
      in case unit of "cm" -> hgtNum >= 150 && hgtNum <= 193 where hgtNum = (read::String->Int) hgt
                      "in" -> hgtNum >= 59 && hgtNum <= 76 where hgtNum = (read::String->Int) hgt
                      _ -> False

isValidHcl::String -> Bool
isValidHcl ('#':rest) = length rest == 6 && all (`elem` (['a'..'f']++['0'..'9'])) rest
isValidHcl _ = False

isValidEcl::String -> Bool
isValidEcl s = s `isInfixOf` "amb blu brn gry grn hzl oth"

isValidPid::String -> Bool
isValidPid s = length s == 9 && all (`elem` ['0'..'9']) s
