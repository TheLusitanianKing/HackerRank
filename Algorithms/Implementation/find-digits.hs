main = interact $ unlines . map (show . solve) . tail . lines

solve :: String -> Int
solve l = foldl helper 0 l
    where helper acc x =
            let n = read [x] in
            if n /= 0 && read l `mod` n == 0
                then acc + 1 else acc