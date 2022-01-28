import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Fibs = Map Integer Integer

modulo :: Integral a => a
modulo = (10^8) + 7

fib :: Integer -> State Fibs Integer
fib x = state (\m ->
    if M.member x m
        then (m M.! x, m) 
        else go 0 1 0 m)
    where
        go a b n m | n == x    = (res, m')
                   | otherwise = go b (a+b) (n+1) m'
            where m'  = M.insert n res m
                  res = a `mod` modulo

solve :: [Integer] -> [Integer]
solve xs = evalState (mapM fib xs) M.empty

main :: IO ()
main = interact $
    unlines . map show . reverse . solve . map read . reverse . tail . lines