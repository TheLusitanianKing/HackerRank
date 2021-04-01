main = interact $ unlines . map show . (\(maxTopics, nbTeams) -> [maxTopics, nbTeams]) . solve . tail . lines

solve :: [String] -> (Int, Int)
solve ts = (mx, (`div` 2) . length . filter (==mx) $ topics)
    where
        mx = maximum topics
        topics = [ a `subjects` b | a <- ts, b <- ts, a /= b ]

subjects :: String -> String -> Int
subjects a b = foldl (\acc x -> if x == 1 then acc + 1 else acc) 0 l
    where
        l = zipWith (\x y -> if x == '1' || y == '1' then 1 else 0) a b