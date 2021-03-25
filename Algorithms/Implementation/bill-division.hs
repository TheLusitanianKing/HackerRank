import Control.Monad (replicateM)

main = do
    [[_, k], items, charged] <- map (map read . words) <$> replicateM 3 getLine
    case rectifier k items (head charged) of
        Just n  -> print n
        Nothing -> putStrLn "Bon Appetit"

rectifier :: Int -> [Int] -> Int -> Maybe Int
rectifier k items charged
    | extra == 0 = Nothing
    | otherwise  = Just extra
    where extra = charged - (value k items `div` 2)

value :: Int -> [Int] -> Int
value k items = sum $ take k items ++ drop (k+1) items