gcd' :: Integral a => a -> a -> a
gcd' 0 m = m
gcd' n 0 = n
gcd' n m = gcd' m (n `mod` m)