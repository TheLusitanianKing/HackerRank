import Control.Monad (forM_, replicateM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (sortBy)

main :: IO ()
main = do
    getLine
    as <- buffer . sortBy (flip compare) . map read . words <$> getLine
    n  <- read <$> getLine
    ts <- map read <$> replicateM n getLine
    forM_ ts $ \t -> do
        print $ solve as t

buffer :: (Integral a) => [a] -> Map a a
buffer = go M.empty 0 1
    where go b _   _  []     = b
          go b acc it (x:xs) = go b' (acc+x) (it+1) xs
            where b' = M.insert (acc+x) it b

solve :: (Integral a) => Map a a -> a -> a
solve buffer t = maybe (-1) snd (M.lookupGE t buffer)