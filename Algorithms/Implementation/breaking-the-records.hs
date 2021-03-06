main = interact $ (\(best, worst) -> show best ++ " " ++ show worst) . total . map read . words . last . lines

total :: [Int] -> (Int, Int)
total (first:scores) = helper (first, 0) (first, 0) scores
    where helper best worst [] = (snd best, snd worst)
          helper best worst (x:xs)
            | x < fst worst = helper best (x, snd worst + 1) xs
            | x > fst best  = helper (x, snd best + 1) worst xs
            | otherwise     = helper best worst xs