import Control.Monad (replicateM)

main :: IO ()
main = do
    t  <- read <$> getLine
    rs <- replicateM t $ do
        n            <- read <$> getLine
        associations <- map (duple . map read . words)
                            <$> replicateM n getLine
        return $ if valid associations then "YES" else "NO"
    putStrLn $ unlines rs

valid :: [(Int, Int)] -> Bool
valid xs = all p xs
    where
        p (i, o) = case lookup i xs of
                        Nothing -> True
                        Just o' -> o == o'

duple :: [Int] -> (Int, Int)
duple [x, y] = (x, y)
duple _      = error "Could not transform the array to a pair"