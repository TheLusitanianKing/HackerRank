main = interact $ unlines
    . map (show . (\[n, m, s] -> ((s + m - 2) `mod` n) + 1) . map read . words)
    . tail
    . lines