main = interact $ show . (\[b:_, ks, ds] -> spent b ks ds) . map (map read . words) . lines

spent :: Int -> [Int] -> [Int] -> Int
spent b ks ds
    | null cbs = -1
    | otherwise = maximum cbs
    where cbs = [ k + d | k <- ks, d <- ds, k + d <= b ]