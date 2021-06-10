main :: IO ()
main = interact $ unlines . map show . solve . map read . lines

solve :: [Int] -> [Int]
solve = map snd . filter (\(i, _) -> odd i) . zip [0..]