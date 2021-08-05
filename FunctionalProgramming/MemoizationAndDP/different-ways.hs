main :: IO ()
main = interact
    $ unlines
    . map (show . (`mod` modulo) . (\[n, k] -> choose n k) . map read . words)
    . tail
    . lines

modulo :: Integer
modulo = 10^8 + 7

choose :: Integral a => a -> a -> a
choose n k = go 1 1 n
    where go acc d n'
            | k > n     = 0
            | d > k     = acc
            | otherwise = go ((acc * n') `div` d) (d+1) (n'-1)