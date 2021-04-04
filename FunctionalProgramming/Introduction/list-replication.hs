main = interact $ unlines
        . (\(x : l) -> concatMap (replicate (read x)) l)
        . lines