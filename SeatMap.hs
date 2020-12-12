module SeatMap
    ( SeatPos,
      SeatMap,
      SeatState (Floor, Empty, Occupied),
      Seat,
      getSeatMap,
      printSeatMap) where

import qualified Data.Map as Map

recSplitAt::Int -> [t] -> [[t]]
recSplitAt _ [] = []
recSplitAt i l =
  let (h, rest) = splitAt i l
   in h : recSplitAt i rest

printSeatMap:: Int -> SeatMap -> IO ()
printSeatMap lineWidth seatMap = do
    putStrLn $ unlines $ map (\x -> foldl1 (++) (show <$> x)) (recSplitAt lineWidth $ Map.elems seatMap)
    return ()

getSeatMap::[String] -> SeatMap
getSeatMap input =
  Map.fromList $ concat $ zipWith getSeats input [0..]

getSeats:: [Char] -> Int -> [Seat]
getSeats row rowNumber =
  zip [[rowNumber, x] | x <- [0..(length row)]] (map (\x -> read [x]) row)

type SeatMap = Map.Map SeatPos SeatState
type Seat = (SeatPos, SeatState)
type SeatPos = [Int]
data SeatState = Floor | Empty | Occupied deriving (Eq)

instance Show SeatState where
  show Floor = "."
  show Empty = "L"
  show Occupied = "#"

instance Read SeatState where
  readsPrec _ (x:xs) =
    let a = case x of '.' -> Floor
                      'L' -> Empty
                      '#' -> Occupied
    in [(a,xs)]
