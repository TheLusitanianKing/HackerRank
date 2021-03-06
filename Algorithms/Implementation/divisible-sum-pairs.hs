main = do
    [_, k] <- map read . words <$> getLine
    xs     <- map read . words <$> getLine
    print $ total k xs

total :: Int -> [Int] -> Int
total k = length . filter (\(x, y) -> (x + y) `mod` k == 0) . pairs

pairs :: [Int] -> [(Int, Int)]
pairs xs = helper [] xs
    where helper acc []     = acc
          helper acc (x:xs) = helper (acc ++ [(a,b) | a <- [x], b <- xs]) xs