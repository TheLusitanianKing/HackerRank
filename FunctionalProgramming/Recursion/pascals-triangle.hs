main :: IO ()
main = do
    b <- read <$> getLine
    putStrLn . unlines . map (unwords . map show) . triangle $ b

factorial :: (Integral a) => a -> a
factorial n = product [1 .. n]

pascal :: (Integral a) => a -> a -> a
pascal row col = factorial row `div` (factorial col * factorial (row - col))

triangle :: (Integral a) => a -> [[a]]
triangle bound = go [] 0
  where
    go acc row
        | row >= bound = acc
        | otherwise    = go acc' (row + 1)
      where
        col  = row + 1
        acc' = acc ++ [map (pascal row) [0 .. col-1]]