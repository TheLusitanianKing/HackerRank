main = interact $ show . (\[s, k] -> superDigit s (read k)) . words

superDigit :: String -> Int -> Int
superDigit s k
    | length s > 1 = let sum = foldl (\acc n -> acc + read [n]) 0 s in superDigit (show $ sum * k) 1
    | otherwise    = read s