import Control.Monad (replicateM)

main = do
    [[s, t], [a, b], as, os] <-
        map (map read . words) . (\x -> take 2 x ++ drop 3 x) <$> replicateM 5 getLine
    putStrLn . unlines . map show $ [count s t a as, count s t b os]

count :: Int -> Int -> Int -> [Int] -> Int
count s t tree = length . filter (\n -> n >= s && n <= t) . map (tree +)