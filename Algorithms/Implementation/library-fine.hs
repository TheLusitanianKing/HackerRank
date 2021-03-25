main = interact $ show
    . (\[d1:m1:y1:_, d2:m2:y2:_] -> solve d1 m1 y1 d2 m2 y2)
    . map (map read . words)
    . lines

solve :: Int -> Int -> Int -> Int -> Int -> Int -> Int
solve d1 m1 y1 d2 m2 y2
    | y1 > y2 = 10000
    | y1 == y2 && m1 > m2 = 500 * (m1 - m2)
    | y1 == y2 && m1 == m2 && d1 > d2 = 15 * (d1 - d2)
    | otherwise = 0