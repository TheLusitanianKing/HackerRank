import Control.Monad (replicateM)
import Data.List (group, nub, sort)

main :: IO ()
main = do
    t <- read <$> getLine
    rs <- replicateM t $ do
        [_, k] <- map read . words <$> getLine
        ns     <- map read . words <$> getLine
        return $ solve k ns
    mapM_ (putStrLn . unwords . map show) rs

solve :: (Integral a, Integral b, Eq b) => a -> [b] -> [b]
solve k ns | null rs   = [-1]
           | otherwise = nub $ filter (`elem` rs) ns
    where
        rs = if null candidates then [] else map head candidates
        candidates = filter ((>= k) . fromIntegral . length) . group . sort $ ns