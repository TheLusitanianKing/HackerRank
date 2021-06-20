main :: IO ()
main = interact $ unlines . map (unwords . rotate) . tail . lines

rotate :: (Eq a) => [a] -> [[a]]
rotate xs = go [] (length xs) xs
  where
    go acc n xs
        | null xs   = [[]]
        | n <= 0    = acc
        | otherwise = go (xs:acc) (n-1) rotatedXs
      where
        rotatedXs = last xs : init xs