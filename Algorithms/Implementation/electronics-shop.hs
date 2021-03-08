main = do
    (b:_) <- map read . words <$> getLine
    ks    <- map read . words <$> getLine
    ds    <- map read . words <$> getLine
    print $ spent b ks ds

spent :: Int -> [Int] -> [Int] -> Int
spent b ks ds
    | null cbs = -1
    | otherwise = maximum cbs
    where cbs = [ k + d | k <- ks, d <- ds, k + d <= b ]