main = interact $ unlines . map (resolve . map read . words) . tail . lines

resolve :: [Int] -> String
resolve (a:b:c:_)
    | distanceA < distanceB = "Cat A"
    | distanceA > distanceB = "Cat B"
    | otherwise             = "Mouse C"
    where distanceA = abs $ a - c
          distanceB = abs $ b - c