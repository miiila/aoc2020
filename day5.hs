import Data.List

main = do
    contents <- readFile "day5_input"
    let input = lines contents
    -- Part 1
        mappedInput = map ((\(x,y) -> (x * 8) + y) . parseInput) input
    print $ foldl max 0 mappedInput
    -- Part 2
    let sorted = sort mappedInput
    print $ [(head sorted)..(last sorted)] \\ sorted

parseInput::String -> (Int, Int)
parseInput s =
  let row = parseRow (take 7 s) 0 127
      col = parseSeat (drop 7 s) 0 7
   in (row, col)


-- TODO: Those functions are identical, compose them
parseRow::String -> Int -> Int -> Int
parseRow [] low _ = low
parseRow (s:xs) low high =
  case s of 'F' -> parseRow xs low bound
            'B' -> parseRow xs (bound+1) high
            where bound = (low + high) `div` 2

parseSeat::String -> Int -> Int -> Int
parseSeat [] low _ = low
parseSeat (s:xs) low high =
  case s of 'L' -> parseSeat xs low bound
            'R' -> parseSeat xs (bound+1) high
            where bound = (low + high) `div` 2
