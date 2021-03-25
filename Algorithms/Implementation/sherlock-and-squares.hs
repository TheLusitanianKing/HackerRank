main = interact $ unlines
    . map (show . (\[a, b] -> solve a b) . map read . words)
    . tail
    . lines

solve :: Integer -> Integer -> Integer
solve a b = helper 0 a square
    where helper acc n (x:xs)
            | x > b = acc
            | otherwise = helper (if x >= a then acc+1 else acc) (n + 1) xs

square :: [Integer]
square = map (^2) [1..]