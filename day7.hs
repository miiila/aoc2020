import SplitOn
import Data.List
import Data.Maybe
import qualified Data.Map as Map

main = do
    contents <- readFile "day7_input"
    let input = lines contents
    -- Part 1
    let bags = Map.fromList $ getBagRulePairFromRow <$> input
        res = length $ filter (\x -> canContainBag x "shiny gold" bags) $ Map.keys bags \\ ["shiny gold"]
    print res
    -- Part 2
    let res = countBags "shiny gold" bags - 1
    print res

canContainBag::String -> String -> Map.Map String [BagRule] -> Bool
canContainBag start needle bags
    | start == needle = True
    | otherwise =
    let rules = Map.lookup start bags
     in case rules of (Just []) -> False
                      (Just r) -> any (\x -> canContainBag (bagName x) needle bags) r

countBags::String -> Map.Map String [BagRule] -> Int
countBags bag bags =
    let rules = Map.lookup bag bags
     in case rules of (Just []) -> 1
                      (Just r) ->  1 + sum ((\x -> capacity x * countBags (bagName x) bags) <$> r)


type BagName = String
data BagRule = BagRule {capacity:: Int, bagName:: BagName} deriving (Show)

getBagRulePairFromRow::String -> (BagName, [BagRule])
getBagRulePairFromRow s =
    let name = getName s
        rules = unwords . drop 4 $ words s
     in (name, getBagRulesFromRow rules)

getBagRulesFromRow::String -> [BagRule]
getBagRulesFromRow [] = []
getBagRulesFromRow s =
    let r = getBagRuleFromString . dropWhile (== ' ') <$> splitOn s ','
     in case r of [Nothing] -> []
                  s -> fromJust <$> s

getBagRuleFromString::String -> Maybe BagRule
getBagRuleFromString s
  | "no" `isPrefixOf` s = Nothing
  | otherwise =
      let (c,_:name) = break (==' ') s
       in Just (BagRule ((read::String->Int) c) (getName name))

getName:: String -> String
getName s =
    unwords . take 2 $ words s
