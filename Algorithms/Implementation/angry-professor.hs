main = interact $ unlines . go . map (map read . words) . tail . lines

go :: [[Int]] -> [String]
go = helper []
    where helper acc [] = acc
          helper acc ((_:k:_):stds:r) = helper (acc ++ [n]) r
            where n | onTime >= k = "NO"
                    | otherwise   = "YES"
                  onTime = length (filter (<= 0) stds)