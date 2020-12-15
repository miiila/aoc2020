import Data.Maybe
import Data.List
import qualified Data.Map as Map

main = do
    let input = [0,13,1,16,6,17]
    -- Part 1
        state = Map.fromList $ zip input ((: []) <$> [1..])
        len = length input + 1
        res = foldl' (\(s,l) c -> play s l c) (state, last input)  [len..2020]
    print $ snd res
    -- Part 2
    let res = foldl' (\(s,l) c -> play s l c) (state, last input)  [len..30000000]
    print $ snd res

play::State -> Int -> Int -> (State, Int) 
play state last turn =
  let turns = fromMaybe [] $ Map.lookup last state
   in case length turns of 1 -> (Map.insertWith (\new old -> [head new, head old]) 0 [turn] state, 0)
                           _ -> let last = foldr1 (-) turns
                                 in (Map.insertWith (\new old -> [head new, head old]) last [turn] state, last)

type State = Map.Map Int [Int]
