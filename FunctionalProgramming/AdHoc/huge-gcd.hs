import Control.Monad (replicateM)

modulo :: Integer
modulo = 10^9 + 7

main :: IO ()
main = do
    [a, b] <- replicateM 2 $ do
        getLine
        product . map read . words <$> getLine
    print . (`mod` modulo) $ gcd a b