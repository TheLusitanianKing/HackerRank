main = do
    [s, t] <- map read . words <$> getLine
    [a, b] <- map read . words <$> getLine
    getLine -- ignored line
    apples <- map read . words <$> getLine
    oranges <- map read . words <$> getLine
    print $ count s t a apples
    print $ count s t b oranges

count :: Int -> Int -> Int -> [Int] -> Int
count s t tree = length . filter (\n -> n >= s && n <= t) . map (tree +)