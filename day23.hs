import Debug.Trace
import Data.Maybe
import qualified Data.Map as Map

main = do
    let input = [1,3,5,4,6,8,7,2,9]
        --input = [3,8,9,1,2,5,4,6,7]
    -- Part 1
        nextList = Map.fromList $ zip input (drop 1 $ concat $ repeat input)
        current = head input
        resList = foldList current nextList 9 !! 99
    print $ concat $ show <$> drop 1 (take 9 (getList 1 resList))
    -- Part 2
    let newInput = input ++ [10..1000000]
        nextList = Map.fromList $ zip newInput (drop 1 $ concat $ repeat newInput)
        resList = foldList current nextList 1000000 !! 9999999
        afterOne = fromJust $ Map.lookup 1 resList
        afterOneOne = fromJust $ Map.lookup afterOne resList
    print $ afterOne*afterOneOne


foldList::Int -> LinkedList -> Int -> [LinkedList]
foldList current linkedList maxNum =
  let newList = iterateList current linkedList maxNum
      newCurrent = fromJust $ Map.lookup current newList
   in newList:foldList newCurrent newList maxNum

iterateList::Int -> LinkedList -> Int -> LinkedList
iterateList current linkedList maxNum = 
    let nextThree = getNextThree current linkedList
        dest = findDest (current-1) nextThree maxNum
        newList = updateNextList current dest nextThree linkedList
     in newList

updateNextList::Int -> Int -> [Int] -> LinkedList -> LinkedList
updateNextList current destination nextThree nextList =
  let newCurrent = fromJust $ Map.lookup (last nextThree) nextList
      newNext = fromJust $ Map.lookup destination nextList
      newNextList = Map.insert current newCurrent nextList
      mapWithDest = Map.insert destination (head nextThree) newNextList
      mapWithThree = Map.insert (last nextThree) newNext mapWithDest
   in mapWithThree

getNextThree::Int -> LinkedList -> [Int]
getNextThree current nextList =
  drop 1 $ take 4 $ getList current nextList

getList::Int -> LinkedList -> [Int]
getList start nextList = iterate (\x -> fromJust $ Map.lookup x nextList) start

type LinkedList = Map.Map Int Int

findDest::Int -> [Int] -> Int -> Int
findDest candidate triplet maxNum
  | candidate == 0 = findDest maxNum triplet maxNum
  | candidate `notElem` triplet = candidate
  | otherwise = findDest (candidate - 1) triplet maxNum
