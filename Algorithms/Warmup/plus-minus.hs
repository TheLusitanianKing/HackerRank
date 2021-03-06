import Numeric (showFFloat)
main = interact $ plusMinus . map read . words . last . lines
    where plusMinus xs = ns (> 0) xs ++ "\n" ++ ns (< 0) xs ++ "\n" ++ ns (== 0) xs
          ns f xs = showFFloat (Just 6) (nb f xs) ""
          nb f xs = (fromIntegral . length . filter f $ xs) / (fromIntegral . length $ xs)