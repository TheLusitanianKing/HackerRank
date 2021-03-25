main = interact $ show
    . (\[_:k:_, hs] -> let m = maximum hs in if m > k then m - k else 0)
    . map (map read . words)
    . lines