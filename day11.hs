import Data.Maybe
import Data.List
import qualified Data.Map as Map

import SeatMap

main = do
    contents <- readFile "day11_input"
    let input = lines contents
        seatMap = getSeatMap input
    -- Part 1
        stableSeating = findStableSeating seatMap iterateSeatMap
        res = length $ Map.filter (== Occupied) stableSeating
    print res
    -- Part 2
    --mapM_ (printSeatMap (length $ head input)) (take 5 $ iterate iterateSeatMapSecond seatMap)
    let stableSeating = findStableSeating seatMap iterateSeatMapSecond
        res = length $ Map.filter (== Occupied) stableSeating
    print res

findStableSeating:: SeatMap -> (SeatMap -> SeatMap) -> SeatMap
findStableSeating seatMap f
  | seatMap == newSeatMap = newSeatMap
  | otherwise = findStableSeating newSeatMap f
  where newSeatMap = f seatMap

iterateSeatMap:: SeatMap -> SeatMap
iterateSeatMap seatMap =
  Map.fromList $ (\x -> getNewSeatState x seatMap 4 getAdjacentSeatsStates) <$> Map.toList seatMap

iterateSeatMapSecond:: SeatMap -> SeatMap
iterateSeatMapSecond seatMap =
  Map.fromList $ (\x -> getNewSeatState x seatMap 5 getDiagonalSeatsStates) <$> Map.toList seatMap

getNewSeatState:: Seat -> SeatMap -> Int -> SeatsF -> Seat
getNewSeatState (p, Floor) _ _ _= (p, Floor)
getNewSeatState (p, s) m i f =
  let adjStates = f p m
      newState =
        case s of Empty -> if Occupied `elem` adjStates then Empty else Occupied
                  Occupied -> if length (Occupied `elemIndices` adjStates) >= i then Empty else Occupied
    in (p, newState)

type SeatsF = SeatPos -> SeatMap -> [SeatState]

getAdjacentSeatsStates:: SeatPos -> SeatMap -> [SeatState]
getAdjacentSeatsStates seat seatMap =
  let pos = [[x,y] | x <- [-1..1], y <- [-1..1]] \\ [[0,0]]
      lookups = map (\x -> zipWith (+) x seat) pos
        in mapMaybe (`Map.lookup` seatMap) lookups

getDiagonalSeatsStates:: SeatPos -> SeatMap -> [SeatState]
getDiagonalSeatsStates seat seatMap =
  let pos = [[x,y] | x <- [-1..1], y <- [-1..1]] \\ [[0,0]]
      lookups = map (getDiagonalSeatsState seat seatMap) pos
        in catMaybes lookups

getDiagonalSeatsState:: SeatPos -> SeatMap -> [Int] -> Maybe SeatState
getDiagonalSeatsState seat seatMap direction =
  let iterator = drop 1 $ iterate (zipWith (+) direction) seat
      a = find (/=Just Floor) [Map.lookup x seatMap | x <- iterator]
   in fromJust a
