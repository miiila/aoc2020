import SplitOn
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

main = do
    contents <- readFile "day19_input"
    --contents <- readFile "day19_input_test"
    let rules:messages = splitOn (lines contents) ""
     -- Part 1
        (r,d) = parseRules rules
        computedRules = computeRules (r,d) 0
        rulesForZero = fromJust $ Map.lookup 0 (snd computedRules)
        res = filter (`satisfy` rulesForZero) $ head messages
    print $ length res
    -- Part 2
    let newR = r
        computedRules = computeRules (newR, d) 0
        rules8 = fromJust $ Map.lookup 8 (snd computedRules)
        rules11 = fromJust $ Map.lookup 11 (snd computedRules)
        split = length $ head rules8
    print $ length $ filter (satisfySecond split (rules8, rules11)) $ head messages

satisfy::String -> [String] -> Bool
satisfy message rules = message `elem` rules

satisfySecond::Int -> ([String], [String]) -> String -> Bool
satisfySecond split (rules8, rules11) message
  | lenSecond < length (head rules11) = False
  | otherwise = let sat8 = satisfyRec8 (take split message) rules8
                    sat11 = satisfyRec11 (drop split message) rules11
                  in if sat8 && sat11
                        then True
                        else satisfySecond (split+length (head rules8)) (rules8, rules11) message
    where lenSecond = length message - split

satisfyRec8::String -> [String] -> Bool
satisfyRec8 [] _ = True
satisfyRec8 message rules = 
  let (messagePart,rest) = splitAt (length $ head rules) message
  in if messagePart `elem` rules
        then satisfyRec8 rest rules
        else False

satisfyRec11::String -> [String] -> Bool
satisfyRec11 [] _ = True
satisfyRec11 message rules = 
  let len = (length $ head rules)
      lI = (length message `div` 2) - (len `div` 2) 
      (lRest, rm) = splitAt lI message
      (m, hRest) = splitAt len rm
  in if satisfy m rules
        then satisfyRec11 (lRest++hRest) rules
        else False

computeRules::Rules -> Int -> Rules
computeRules (ref, def) prevDefSize
  | null ref || prevDefSize == currentDefSize  = (ref, def)
  | otherwise = computeRules (iterateRules (ref,def)) (Map.size def)
  where currentDefSize = Map.size def

iterateRules::Rules -> Rules
iterateRules (ref, def) =
  let newR = getResolvedRules (ref,def)
      newD = updateRules (newR, def)
   in (ref Map.\\ newR, newD)

updateRules::Rules -> RulesDef
updateRules (r,d) = 
  let a = (\x -> map (fromJust . (`Map.lookup` d)) <$> x) <$> r 
      b = concat <$> map (foldl1 (\acc c -> (++) <$> acc <*> c)) <$> a
   in Map.union b d

getResolvedRules::Rules -> RulesRef
getResolvedRules (rulesRef,rulesDef) = 
  let resolvedKeys = Map.keysSet rulesDef
   in Map.filter (\val -> null $ Set.fromList (concat val) Set.\\ resolvedKeys) rulesRef

parseRules::[String] -> Rules
parseRules rules = foldl parseRule (Map.empty, Map.empty) rules

parseRule::Rules -> String -> Rules
parseRule (ref, def) rule = 
  let (r,s) = break(==':') rule
      rul = drop 2 s
      k = read r
   in if head rul == '"'
         then (ref, Map.insert k [[rul !! 1]] def)
         else let w = splitOn (words rul) "|"
               in (Map.insert k (map read <$> w) ref, def)

type Rules = (RulesRef, RulesDef)
type RulesRef = Map.Map Int [[Int]]
type RulesDef = Map.Map Int [String]
