main :: IO ()
main = interact $ unwords
                . map show
                . tuplesToList
                . gcd'
                . map (listToTuples . map read . words)
                . tail
                . lines

listToTuples :: [a] -> [(a, a)]
listToTuples []       = []
listToTuples [x]      = error "Could not construct the tuples"
listToTuples (x:y:xs) = (x, y):listToTuples xs

tuplesToList :: [(a, a)] -> [a]
tuplesToList []          = []
tuplesToList ((x, y):xs) = x:y:tuplesToList xs

fusion :: (Integral a) => [(a, a)] -> [(a, a)] -> [(a, a)]
fusion xs ys = filter ((/= 0) . snd) . map fusionWithY $ xs
    where fusionWithY (prime, power) =
            case lookup prime ys of
                Nothing     -> (prime, 0)
                Just power' -> (prime, min power power')
          
gcd' :: (Integral a) => [[(a, a)]] -> [(a, a)]
gcd' = foldr1 fusion