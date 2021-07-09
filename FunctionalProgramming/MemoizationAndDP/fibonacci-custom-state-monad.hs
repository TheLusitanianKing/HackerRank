{-# LANGUAGE TupleSections #-}

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Fibs = Map Integer Integer
type State = Fibs
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap f st = S (\s ->
            let (r, s') = app st s in (f r, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    -- pure x = S (\s -> (x, s))
    pure x = S (x,)

    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    (<*>) stf st = S (\s ->
            let (f, s')  = app stf s
                (r, s'') = app st s'
            in (f r, s''))
        
instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->
            let (r, s') = app st s in app (f r) s')

modulo :: Integral a => a
modulo = (10^8) + 7

fib :: Integer -> ST Integer
fib x =
    S (\m -> if M.member x m then (m M.! x, m) else go 0 1 0 m)
    where
        go a b n m | n == x    = (res, m')
                   | otherwise = go b (a+b) (n+1) m'
            where m'  = M.insert n res m
                  res = a `mod` modulo

solve :: [Integer] -> [Integer]
solve xs = fst $ app (mapM fib xs) M.empty

main :: IO ()
main = interact $
    unlines . map show . reverse . solve . map read . reverse . tail . lines