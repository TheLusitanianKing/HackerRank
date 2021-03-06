main = interact $ (\[x1, v1, x2, v2] -> meet x1 v1 x2 v2) . map read . words

meet :: Int -> Int -> Int -> Int -> String
meet x1 v1 x2 v2
    | x1 < x2 && v2 > v1   = "NO"
    | x2 < x1 && v1 > v2   = "NO"
    | x1 /= x2 && v1 == v2 = "NO"
    | x1 == x2             = "YES"
    | otherwise            = meet (x1+v1) v1 (x2+v2) v2