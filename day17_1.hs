import SplitOn
import Data.Maybe
import Data.List
import qualified Data.Map as Map

main = do
    contents <- readFile "day17_input"
    --contents <- readFile "day17_input_test"
    let input = lines contents
        cubeMap = getCubeMap input
        res = iterate iterateCubeMap cubeMap !! 6
    print $ length $ Map.filter (=='#') res

iterateCubeMap:: CubeMap -> CubeMap
iterateCubeMap cubeMap =
  let mapMin = minimum $ Map.keys cubeMap
      mapMax = maximum $ Map.keys cubeMap
      newCubeMap = Map.union cubeMap (getMapCone (mapMin, mapMax)) 
   in Map.fromList $ getNewCubeState newCubeMap <$> Map.toList newCubeMap

getCubeMap::[String] -> CubeMap
getCubeMap input =
  Map.fromList $ concat $ zipWith getCubes input [0..]

getCubes:: [Char] -> Int -> [Cube]
getCubes row rowNumber =
  zip [(rowNumber, x, 0) | x <- [0..(length row)]] row

getMapCone:: ((Int,Int, Int), (Int,Int, Int)) -> CubeMap
getMapCone  ((xl,yl,zl),(xh, yh,zh)) =
  Map.fromList $ zip [(x,y,z) | x <- [xl-1..xh+1], y <- [yl-1..yh+1], z <- [zl-1..zh+1]] (repeat '.')

getAdjacentCubes:: Coords -> CubeMap -> [Char]
getAdjacentCubes cube cubeMap =
  let pos = [(x,y,z) | x <- [-1..1], y <- [-1..1], z <- [-1..1]] \\ [(0,0,0)]
      lookups = map (+ cube) pos
        in map (fromMaybe '.' . (`Map.lookup` cubeMap)) lookups

getNewCubeState:: CubeMap -> Cube -> Cube
getNewCubeState  cubeMap (coords, state) =
  let adjStates = getAdjacentCubes coords cubeMap
      activeAround = length ('#' `elemIndices` adjStates)
      newState =
        case state of '#' -> if activeAround == 2 || activeAround == 3 then '#' else '.'
                      '.' -> if activeAround == 3 then '#' else '.'
    in (coords, newState)

type CubeMap = Map.Map Coords Char

type Coords = (Int, Int, Int)

instance (Num a,Num b, Num c) => Num (a, b, c) where
   (a,b,c) + (d,e,f) = (a+d,b+e,c+f)

type Cube = (Coords, Char)
