main :: IO ()
main = do
    x <- read <$> getLine
    n <- read <$> getLine
    print . powers x $ n

powers :: (Integral a) => a -> a -> Int
powers x n =
    count x [y ^ n | y <- [1 .. fromIntegral upperBound]]
  where
    upperBound = ceiling $ fromIntegral x ** (1 / fromIntegral n)
    count y [] = 0
    count y (c : cs)
        | y == c = 1
        | c > y = 0
        | otherwise = count (y - c) cs + count y cs