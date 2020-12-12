main = do
    contents <- readFile "day12_input"
    let input = (\(x:xs) -> (x, read xs::Int)) <$> lines contents
        ship = (0,0)
        pos = (ship, 'E')
    -- Part 1
        (lastPos, _) = foldl handleIns pos input
        res = abs (fst lastPos) + abs ( snd lastPos)
    print res
    -- Part 2
    let wpt = (10, 1)
        (lastPos,_) = foldl handleInsPart2 (ship, wpt) input
        res = abs (fst lastPos) + abs ( snd lastPos)
    print res

handleInsPart2::PosWpt -> Instruction -> PosWpt
handleInsPart2 (ship, wpt) ins =
  if isRel ins
     then (ship, handleRel wpt ins)
     else let (c, i) = ins in case c of 'F' -> handleMovToWpt (ship, wpt) i
                                        'L' -> (ship, handleWptLeft wpt i)
                                        'R' -> (ship, handleWptRight wpt i)

handleMovToWpt::PosWpt -> Int -> PosWpt
handleMovToWpt (ship, wpt) d =
  let newShip = (fst ship + d * fst wpt, snd ship + d * snd wpt)
   in (newShip, wpt)

handleWptLeft::Coords -> Int -> Coords
handleWptLeft (x,y) 0 = (x, y)
handleWptLeft (x,y) 90 = (-y, x)
handleWptLeft (x,y) 180 = (-x, -y)
handleWptLeft (x,y) 270 = (y, -x)

handleWptRight::Coords -> Int -> Coords
handleWptRight c d = handleWptLeft c (360-d)


handleIns::Pos -> Instruction -> Pos
handleIns (coords, dir) ins =
  if isRel ins
     then (handleRel coords ins, dir)
     else handleMov (coords, dir) ins

handleMov::Pos -> Instruction -> Pos
handleMov (coords, dir) (c, v) =
  case c of 'F' -> (handleRel coords (dir, v), dir)
            'L' -> (coords, handleLeftTurn dir v)
            'R' -> (coords, handleRightTurn dir v)

handleLeftTurn::Direction -> Int -> Direction
handleLeftTurn 'N' 0 = 'N'
handleLeftTurn 'N' 90 = 'W'
handleLeftTurn 'N' 180 = 'S'
handleLeftTurn 'N' 270 = 'E'
handleLeftTurn 'N' 360 = 'N'
handleLeftTurn 'E' v = handleLeftTurn 'N' (v-90)
handleLeftTurn 'S' v
  | v >= 180 = handleLeftTurn 'N' (v-180)
  | v < 180 = handleLeftTurn 'N' (v+180)
handleLeftTurn 'W' v = handleLeftTurn 'N' (v+90)

handleRightTurn::Direction -> Int -> Direction
handleRightTurn d v = handleLeftTurn d (360-v)

handleRel::Coords -> Instruction -> Coords
handleRel (x, y) (c,v) =
  case c of 'N' -> (x, y+v)
            'S' -> (x, y-v)
            'E' -> (x+v, y)
            'W' -> (x-v, y)

isRel::Instruction -> Bool
isRel (i,_) =
  i `elem` "NSEW"

type Instruction = (Char, Int)
type Coords = (Int, Int)
type Pos = (Coords, Direction)
type PosWpt = (Coords, Coords)
type Direction = Char
