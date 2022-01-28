main :: IO ()
main = interact $ show . (\[n, k] -> solve n k) . map read . words

solve :: (Integral a, Read a, Show a) => a -> a -> a
solve n k = superDigit $ k * (sum . map (\c -> read [c]) . show $ n)

superDigit :: (Integral a, Read a, Show a) => a -> a
superDigit x
    | length (show x) == 1 = x
    | otherwise            = superDigit . digitSum $ x

digitSum :: (Integral a, Read a, Show a) => a -> a
digitSum = sum . map (\c -> read [c]) . show