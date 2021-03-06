import Data.List (subsequences)

main = interact $ (\(x, y) -> show x ++ " " ++ show y) . minmax . map read . words

minmax :: [Int] -> (Int, Int)
minmax xs = (minimum ss, maximum ss)
    where ss = map sum . filter (\x -> length x == 4) . subsequences $ xs