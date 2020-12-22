import SplitOn
import qualified Data.Set as Set
import Data.List
import Debug.Trace

main = do
    contents <- readFile "day22_input"
    let input = lines contents
    -- Part 1
        decks = getCardDecks input
        res = calculateScore $ playGame decks
    print res 
    -- Part 2
    let winner = playGameRecursive decks Set.empty 
        res = calculateScore $ snd winner
    print res

calculateScore::[Int] -> Int
calculateScore deck =
  let scores = zipWith (*) (reverse deck) [1..]
    in sum scores

playGameRecursive::([Int], [Int]) -> Set.Set ([Int], [Int]) -> (Int, [Int])
playGameRecursive (deck1, []) _ = (0, deck1)
playGameRecursive ([], deck2) _ = (1, deck2)
playGameRecursive game seen 
  | Set.member game seen = (0, fst game)
  | otherwise = 
    let newSeen = Set.insert game seen
     in playGameRecursive (playRoundRecursive game) newSeen

playRoundRecursive::([Int], [Int]) -> ([Int], [Int])
playRoundRecursive (x:xs, y:ys) =
  let winner = if x <= length xs && y <= length ys 
           then fst $ playGameRecursive (take x xs, take y ys) Set.empty 
           else if x > y then 0 else 1 
  in case winner of 0 -> (xs++[x,y], ys)
                    1 -> (xs, ys++[y,x])

getCardDecks::[String] -> ([Int], [Int])
getCardDecks input = 
  let sInput = splitOn input ""
      deck1 = read <$> drop 1 (head sInput)
      deck2 = read <$> drop 1 (sInput !! 1)
   in (deck1, deck2) 

playGame::([Int], [Int]) -> [Int]
playGame (deck1, []) = deck1
playGame ([], deck2) = deck2
playGame game = playGame $ playRound game
  

playRound::([Int], [Int]) -> ([Int], [Int])
playRound (x:xs, y:ys)
  | x > y = (xs++[x,y], ys)
  | x < y = (xs, ys++[y,x])
