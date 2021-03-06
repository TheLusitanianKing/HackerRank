main = interact $ unlines . staircase . read

staircase :: Int -> [String]
staircase x = helper [] x
    where helper acc n
            | n <= 0    = acc
            | otherwise = helper (create x n:acc) (n - 1)
          create fullsize n = replicate (fullsize - n) ' ' ++ replicate n '#'