import Control.Monad (replicateM_)

solve :: Int -> Int -> Int -> Int
solve n c m = go 0 0 n
  where
    go eaten packages money
      | money >= c =
        let eat = money `div` c
        in go (eaten + eat) (packages + eat) (money `rem` c)
      | packages < m = eaten
      | otherwise =
        let eat = packages `div` m
        in go (eaten + eat) ((packages `rem` m) + eat) money

main :: IO ()
main = do
  t <- read <$> getLine
  replicateM_ t $ do
    [n, c, m] <- map read . words <$> getLine
    print $ solve n c m