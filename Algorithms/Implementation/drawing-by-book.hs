main = do
    n <- read <$> getLine
    p <- read <$> getLine
    print $ minimum [p `div` 2, n `div` 2 - p `div` 2]