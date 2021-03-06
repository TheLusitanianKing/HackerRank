main = interact $ show . calculate . map (map read . words) . tail . lines
    where calculate m = abs $ leftD m - rightD m
          leftD m = helper m 0 0
                where helper (l:t) i acc
                        | i >= length l - 1 = acc + (l !! i)
                        | otherwise         = helper t (i + 1) (acc + (l !! i))
          rightD = leftD . map reverse