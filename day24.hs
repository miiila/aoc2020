import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Debug.Trace


main = do
    contents <- readFile "day24_input"
    --contents <- readFile "day24_input_test"
    let input = lines contents
    -- Part 1
        pInput = getSeq <$> input
        res = "exists"
        refTile = (0,0)
        tiles = (`getDirection` refTile) <$> pInput
        tilesMap = Map.map (`mod` 2) $ Map.fromListWith (+) $ zip  tiles (repeat 1)
    print $ Map.size $ Map.filter (== 1) tilesMap
    -- Part 2
    let i = iterate iterateTiles tilesMap
    print $ Map.size . Map.filter (== 1) <$> drop 100 (take 101 i)
    print "yes"


iterateTiles::Map.Map (Int, Int) Int -> Map.Map (Int,Int) Int
iterateTiles tilesMap =
  let enl = enlargeFiledTiles tilesMap
   in Map.fromList $ (`getNewTilesState` enl) <$> Map.keys enl 

enlargeFiledTiles::Map.Map (Int, Int) Int -> Map.Map (Int,Int) Int
enlargeFiledTiles tilesMap =
  let filed = Map.filter (== 1) tilesMap
      adjFiled = foldl Map.union tilesMap $ map (`getAdjacentTiles` tilesMap) (Map.keys filed)
   in adjFiled

getAdjacentTiles:: (Int,Int) -> Map.Map (Int,Int) Int -> Map.Map (Int,Int) Int
getAdjacentTiles tile tilesMap =
    let lookups = map (+ tile) [(0,2), (0,-2), (-1,1), (1,-1),(1,1),(-1,-1)]
        l = fromMaybe 0 . (`Map.lookup` tilesMap) <$> lookups
     in Map.fromList $ zip lookups l

getNewTilesState:: (Int,Int) -> Map.Map (Int, Int) Int ->  ((Int,Int), Int)
getNewTilesState tileCoords tilesMap =
  let currentState = fromJust $ Map.lookup tileCoords tilesMap
      blackAround = length (1 `elemIndices` Map.elems (getAdjacentTiles tileCoords tilesMap))
      newState =
        case currentState of 1 -> if blackAround == 0 || blackAround > 2 then 0 else 1 
                             0 -> if blackAround == 2 then 1 else 0 
      in (tileCoords, newState)

getDirection::[String] -> (Int,Int) -> (Int,Int)
getDirection [] dir = dir
getDirection ("e":xs) dir = getDirection xs dir + (0,2)
getDirection ("w":xs) dir = getDirection xs dir + (0,-2)
getDirection ("ne":xs) dir = getDirection xs dir + (-1,1)
getDirection ("sw":xs) dir = getDirection xs dir + (1,-1)
getDirection ("se":xs) dir = getDirection xs dir + (1,1)
getDirection ("nw":xs) dir = getDirection xs dir + (-1,-1)

--data Direction = Direction {se:: Int, e:: Int, ne:: Int} deriving (Show, Eq, Ord)

getSeq::String -> [String]
getSeq [] = []
getSeq (x:xs)
  | x == 'e' = [x]:getSeq xs
  | x == 'w' = [x]:getSeq xs
  | otherwise = [x,head xs]:getSeq (drop 1 xs)

instance (Num a,Num b) => Num (a, b) where
   (a,b) + (c,d) = (a+c,b+d)
