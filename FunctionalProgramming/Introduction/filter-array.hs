main :: IO ()
main = interact $ unlines . map show . solve . map read . lines

solve :: [Int] -> [Int]
solve []     = error "Empty list"
solve [x]    = error "Only the upper bound"
solve (x:xs) = filter (< x) xs