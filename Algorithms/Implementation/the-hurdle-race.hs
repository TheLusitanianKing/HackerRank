main = do
    (_:k:_) <- map read . words <$> getLine
    hs      <- map read . words <$> getLine
    print $ let m = maximum hs in if m > k then m - k else 0