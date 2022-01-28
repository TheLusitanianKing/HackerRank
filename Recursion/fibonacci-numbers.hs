fib :: (Integral a) => [a]
fib = 0 : 1 : zipWith (+) fib (tail fib)

main :: IO ()
main = interact $ show . (\x -> fib !! (x-1)) . read