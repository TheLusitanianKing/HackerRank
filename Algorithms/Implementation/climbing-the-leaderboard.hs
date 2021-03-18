import Data.List (nub)
main = do
    getLine -- ignored
    rs <- words <$> getLine
    getLine -- ignored
    ps <- words <$> getLine
    putStrLn . unlines . map show . rankAll (prepare rs) $ ps

prepare :: [String] -> [(Int, String)] -- (Rank, Score) from lowest to highest
prepare = helper [] 1 ""
    where helper acc _ _ []     = acc
          helper acc n lt (x:xs)
            | lt == x   = helper acc n lt xs
            | otherwise = helper ((n, x):acc) (n+1) x xs

rankAll :: [(Int, String)] -> [String] -> [Int]
rankAll = helper []
    where helper acc _ []          = reverse acc
          helper acc [] (_:ps)     = helper (1:acc) [] ps
          helper acc fr@((r, x):rs) fp@(p:ps)
            | rx > rp  = helper ((r+1):acc) fr ps
            | rx == rp = helper (r:acc) fr ps
            | otherwise = helper acc rs fp
            where rx = (read :: String -> Int) x
                  rp = (read :: String -> Int) p