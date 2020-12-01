import System.IO

main = do
    contents <- readFile "day1_input"
    let input = map (read::String->Int) $ lines contents
    -- Part 1
    let res = head [x*y | x <- input, y <- input, x+y == 2020]
    print res
    -- Part 2
    let res = head [x*y*z | x <- input, y <- input, z <-input, x+y+z == 2020]
    print res
