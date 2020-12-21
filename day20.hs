import SplitOn
import Data.List
import Data.Bits
import Debug.Trace
import Data.Maybe
import qualified Data.Map as Map

main = do
    contents <- readFile "day20_input"
    --contents <- readFile "day20_input_test"
    let input = splitOn (lines contents) ""
    -- Part 1
        tiles = parseTile <$> input
        deck = Map.singleton (0,0) (head tiles)
        queue = Map.fromList $ getPredicatesForCoords (0,0) (head tiles)
        (newDeck, newQueue) = buildDeck (deck, queue) (drop 1 tiles)
        (topRow, topCol) = minimum $ Map.keys newDeck
        (botRow, botCol) = maximum $ Map.keys newDeck
        corners = [(topRow, topCol), (topRow, botCol), (botRow, topCol), (botRow, botCol)]
        res = tileId . fromJust . (`Map.lookup` newDeck) <$> corners 
    print $ product res
    -- Part 2
    let pic = map (content . snd) <$> groupBy (\(a,b) (c,d) -> fst a == fst c) (Map.toList newDeck)
        cropped = map cropTile <$> pic
        ass = assemblyCropped cropped
        rotations = take 4 $ iterate rotateRight ass
        flipped = flipLr <$> rotations
        totalHashes = sum $ length . filter (== '#') <$> ass
        lochnessCount = sum $ length . filter (==True) . findLochness <$> rotations `union` flipped
        res = totalHashes - (lochnessCount * 15)
    print res

findLochness::[String] -> [Bool]
findLochness input =
  [isLochness input x y | x <- [1..(length input -1)], y <- [0..length (head input) - 20]]

isLochness::[String] -> Int -> Int -> Bool
isLochness inp row col= 
  let parts = [containMid (inp !! row) col, containHead (inp !! (row -1)) col, containFeet (inp !! (row +1)) col]
  in all (==True) parts

containFeet::String -> Int -> Bool
containFeet inp ind = 
  let t = [inp !! (ind+1), inp !! (ind+4), inp !! (ind+7), inp !! (ind+10), inp !! (ind+13), inp !! (ind+16)]
   in all (== '#') t

containHead::String -> Int -> Bool
containHead inp ind = (inp !! (ind+18)) == '#'

containMid::String -> Int -> Bool
containMid inp ind
  | ind + 19 > length inp = False
  | otherwise = 
  let t = [inp !! ind, inp !! (ind+5), inp !! (ind+6), inp !! (ind+11), inp !! (ind+12), inp !! (ind+17), inp !! (ind+18), inp !! (ind+19)]
   in all (== '#') t

printAss::[String] -> IO()
printAss inp = do
  mapM_ putStrLn inp
  putStrLn "----"

assemblyCropped::[[[String]]] -> [String]
assemblyCropped inp = 
  let conc = map assemblyCroppedPart inp
   in concat conc

assemblyCroppedPart::[[String]] -> [String]
assemblyCroppedPart = map concat <$> transpose 
  

cropTile::[String] -> [String]
cropTile = cropTop . cropBottom . cropLeft . cropRight

cropTop::[String] -> [String]
cropTop = drop 1

cropBottom::[String] -> [String]
cropBottom = reverse . drop 1 . reverse 

cropLeft::[String] -> [String]
cropLeft = map $ drop 1

cropRight::[String] -> [String]
cropRight = map (reverse . drop 1 . reverse)


buildDeck::(Deck, Queue) -> [Tile] -> (Deck, Queue)
buildDeck (currentDeck, currentQueue) availableTiles
  | null currentQueue = (currentDeck, currentQueue)
  | otherwise =
  let ((coords,p):xs) = Map.toList currentQueue
      matchingTile = findMatchingTile p availableTiles
   in case matchingTile of Nothing -> buildDeck (currentDeck, Map.fromList xs) availableTiles
                           (Just tile) -> 
                              let newQueue = Map.fromList xs
                                  newPredicates = getPredicatesForCoords coords tile
                                  predicatesToAdd = (fst <$> newPredicates) \\ Map.keys currentDeck
                                  updatedQueue = foldl (\acc (k,v) -> Map.insert k v acc) newQueue (filter (\x -> fst x `elem` predicatesToAdd) newPredicates) 
                                  newDeck = Map.insert coords tile currentDeck
                                  newTiles = substractTiles availableTiles tile
                                in buildDeck (newDeck, updatedQueue) newTiles

type Coords = (Int, Int)
type Deck = Map.Map Coords Tile
type Queue = Map.Map Coords (Tile->Bool)

substractTiles::[Tile] -> Tile -> [Tile]
substractTiles tiles tile = filter (\x -> tileId tile /= tileId x) tiles

getPredicatesForCoords:: Coords -> Tile -> [(Coords, Tile->Bool)]
getPredicatesForCoords (x,y) tile =
  let right = ((x,y+1),\x -> getRightBorder tile == getLeftBorder x) 
      left = ((x,y-1),\x -> getLeftBorder tile == getRightBorder x) 
      top = ((x-1,y),\x -> getTopBorder tile == getBottomBorder x) 
      bot = ((x+1,y),\x -> getBottomBorder tile == getTopBorder x) 
    in [right, left, top, bot]


updateDeckWithMatchingTile::(Coords, Tile->Bool) -> Map.Map Coords Tile -> [Tile] -> Map.Map Coords Tile
updateDeckWithMatchingTile (coords, predicate) deck tiles =
  let matchingTile = findMatchingTile predicate tiles
    in case matchingTile of (Just tile) -> Map.insert coords tile deck
                            Nothing -> deck

findMatchingTile::(Tile -> Bool) -> [Tile] -> Maybe Tile
findMatchingTile predicate tiles =
    let allTiles = getAllOptions <$> tiles
        res = find predicate <$> allTiles
     in listToMaybe $ catMaybes res


getAllOptions::Tile -> [Tile]
getAllOptions tile = getAllRotations tile `union` getAllFlippedRotations tile

getAllFlippedRotations::Tile -> [Tile]
getAllFlippedRotations tile =
  getAllRotations $ modifyTile tile flipLr

getAllRotations::Tile -> [Tile]
getAllRotations tile = 
  let rotator = (`modifyTile` rotateRight)
   in take 4 $ iterate rotator tile

modifyTile::Tile -> ([String] -> [String]) -> Tile
modifyTile tile tileFn =
  Tile{tileId=tileId tile, height=height tile, width=width tile, content=tileFn $ content tile}

getTopBorder::Tile -> String
getTopBorder tile = head $ content tile

getBottomBorder::Tile -> String
getBottomBorder tile = last $ content tile

getRightBorder::Tile -> String
getRightBorder tile = last <$> content tile

getLeftBorder::Tile -> String
getLeftBorder tile = head <$> content tile

rotateRight::[String] -> [String]
rotateRight tile = transpose $ flipBt tile

flipLr::[String] -> [String]
flipLr = map reverse

flipBt::[String] -> [String]
flipBt = reverse 

parseTile::[String] -> Tile
parseTile tileStrings =
  -- reverse drop reverse removes last : from id
  let id = read $ reverse $ drop 1 $ reverse $ words (head tileStrings) !! 1
      ct = drop 1 tileStrings 
      hg = length $ tileStrings !! 1
      wg = length tileStrings - 1
   in Tile {tileId=id, height=hg, width=wg, content=ct}  

type Edges = Map.Map String Int
data Tile = Tile {tileId:: Int, height:: Int, width:: Int, content:: [String]} deriving (Show, Eq)

printTilesRows::[[[String]]] -> IO()
printTilesRows = mapM_ printTileRow 

printTileRow::[[String]] -> IO()
printTileRow tiles = do
  let tilesToPrint = map concat <$> transpose $ tiles
  putStrLn $ intercalate ['\n'] tilesToPrint
