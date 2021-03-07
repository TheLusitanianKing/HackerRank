main = do
    [_, k]  <- map read . words <$> getLine
    items   <- map read . words <$> getLine
    charged <- read <$> getLine
    case rectifier k items charged of
        Just n  -> print n
        Nothing -> putStrLn "Bon Appetit"

rectifier :: Int -> [Int] -> Int -> Maybe Int
rectifier k items charged
    | extra == 0 = Nothing
    | otherwise  = Just extra
    where extra = charged - (value k items `div` 2)

value :: Int -> [Int] -> Int
value k items = sum $ take k items ++ drop (k+1) items