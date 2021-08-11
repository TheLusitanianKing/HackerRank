main :: IO ()
main = do
    d <- last . map read . words <$> getLine
    xs <- map read . words <$> getLine
    print . beautifulTriplets d $ xs

beautifulTriplets :: (Integral a) => a -> [a] -> Int
beautifulTriplets d = go 0
  where
    go acc []     = acc
    go acc (x:xs) =
        -- it works with `elem` search function
        -- although there is a better way to do that, because, as it is sorted,
        -- we could stop searching when we go above x+2d to save some time
        let acc' = acc + fromEnum ((x + d) `elem` xs && (x + (2 * d)) `elem` xs)
        in go acc' xs