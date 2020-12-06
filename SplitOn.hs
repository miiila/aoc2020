module SplitOn
    ( splitOn) where

splitOn::(Eq t) => [t] -> t -> [[t]]
splitOn [] _ = []
splitOn s d =
  let (g, rest) = break (== d) s
   in g : splitOn (dropWhile (== d) rest) d
