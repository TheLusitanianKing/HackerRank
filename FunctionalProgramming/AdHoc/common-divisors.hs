main :: IO ()
main = interact $ unlines . map (show . solve . map read . words) . tail . lines

solve :: (Integral a) => [a] -> a
solve [x, y] =
    sum . map f $ [1..upperBound]
    where g = gcd x y
          upperBound = floor . sqrt . fromIntegral $ g
          f n | g `mod` n == 0 = if g `div` n == n then 1 else 2
              | otherwise      = 0         
solve _ = error "Bad input! Should only have 2 values on one line."