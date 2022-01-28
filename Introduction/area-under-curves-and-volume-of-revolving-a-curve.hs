import Control.Monad (replicateM)
import Text.Printf (printf)

step :: Double
step = 0.001

main :: IO ()
main = do
    [as, bs, [l, r]] <- replicateM 3 (map read . words <$> getLine)
    printf "%.1f\n" $ area as bs l r
    printf "%.1f\n" $ volume as bs l r

f :: [Int] -> [Int] -> Double -> Double
f as bs x = sum $ zipWith (\a b -> fromIntegral a * (x ^^ b)) as bs

integral :: (Double -> Double) -> [Int] -> [Int] -> Int -> Int -> Double
integral g as bs l r = sum xs
  where
    ld = fromIntegral l
    rd = fromIntegral r
    xs = map (g . f as bs) (tail [ld, ld + step .. rd])

area, volume :: [Int] -> [Int] -> Int -> Int -> Double
area   = integral (* step)
volume = integral (\x -> pi * (x ^ 2) * step)