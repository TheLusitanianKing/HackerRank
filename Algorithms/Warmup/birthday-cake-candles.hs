main = interact $ show . f . map (read :: String -> Int) . words . last . lines
    where f xs = length $ filter (== maximum xs) xs