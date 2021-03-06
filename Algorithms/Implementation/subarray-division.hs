import Data.List (inits, tails)

main = do
    getLine
    squares <- map read . words <$> getLine
    [d, m]  <- map read . words <$> getLine
    print $ total d m squares

total :: Int -> Int -> [Int] -> Int
total d m = length . filter (\x -> length x == m && sum x == d) . concatMap inits . tails