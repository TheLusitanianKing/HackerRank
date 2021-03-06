main = interact $ show . (\[as, bs] -> total as bs) . map (map read . words) . tail . lines

total :: [Int] -> [Int] -> Int
total as bs = length . filter (\n -> f n as bs) $ [1..maximum bs]
    where f n as bs = all (\a -> n `mod` a == 0) as && all (\b -> b `mod` n == 0) bs