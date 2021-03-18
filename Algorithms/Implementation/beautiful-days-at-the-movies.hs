main = interact $ show . solve . map read . words

solve :: [Int] -> Int
solve (x:y:k:_) = foldl (\acc x -> if difference x `rem` k == 0 then acc + 1 else acc) 0 [x..y]

difference :: Int -> Int
difference x = abs $ x - inverse x
    where inverse = read . reverse . show