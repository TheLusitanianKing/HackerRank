main = interact $ unlines . map show . solve . map read . words . last . lines

solve :: [Int] -> [Int]
solve = helper []
    where
        helper acc stks
            | null stks = reverse acc
            | otherwise = helper (l:acc) stks'
            where l = length stks
                  m = minimum stks
                  stks' = filter (>0) . map (\x -> x-m) $ stks