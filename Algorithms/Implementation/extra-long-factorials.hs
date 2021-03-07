main = do
    n <- (read :: String -> Integer) <$> getLine
    print $ product [1..n]