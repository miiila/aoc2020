main = do
    contents <- readFile "day18_input"
    --contents <- readFile "day18_input_test"
    let input = lines contents
    -- Part 1
        res = solveTop <$> input 
    print $ sum res

solveTop::String -> Int
solveTop s = 
  let r = solveIter (0, (+)) s
    in fst $ fst r 

solveIter::Expr -> String -> (Expr,String)
solveIter c [] = (c,[])
solveIter c s =
   if head s == ')'
      then (c,tail s)
      else  let (newC, newS) = solve c s
        in solveIter newC newS
   

solve::Expr -> String -> (Expr, String)
solve (num, op) (x:xs) =
  case x of '+' -> ((num, (+)), xs)
            '*' -> ((num, (*)), xs)
            ' ' -> ((num, op), xs)
            '(' -> let ((r,s), ys) = solveIter (0, (+)) xs
              in ((op num r, s), ys)
            i -> let res = op num ((read::String->Int) [i]) 
                  in ((res, (+)), xs)

type Expr = (Int, Int->Int->Int)


