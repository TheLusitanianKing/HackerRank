import Data.List (isSuffixOf)

main = interact convert

convert :: String -> String
convert h
    | "AM" `isSuffixOf` h = "0" ++ show (hour h `mod` 12) ++ take (length h - 4) (drop 2 h)
    | "PM" `isSuffixOf` h = show (hour h `mod` 12 + 12) ++ take (length h - 4) (drop 2 h)

hour :: String -> Int
hour = read . take 2