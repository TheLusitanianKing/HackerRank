import Data.List (inits, tails)

main = interact $ show . (\[squares, [d, m]] -> total d m squares) . map (map read . words) . tail . lines

total :: Int -> Int -> [Int] -> Int
total d m = length . filter (\x -> length x == m && sum x == d) . concatMap inits . tails