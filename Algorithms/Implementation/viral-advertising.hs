main = interact $ show . sequ . read 

sequ n = helper 5 0 0
    where helper sh day acc
            | day == n  = acc
            | otherwise = helper (liked * 3) (day + 1) (acc + liked)
            where liked = sh `div` 2