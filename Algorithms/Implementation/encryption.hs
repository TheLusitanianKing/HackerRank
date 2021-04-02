import Data.List (transpose)

main :: IO ()
main = interact encrypt

encrypt :: String -> String
encrypt s =
    let s' = filter (/= ' ') s -- cleaned sentence
        c  = ceiling . sqrt . fromIntegral . length $ s' -- columns length
    in unwords . transpose . separate c $ s'

-- | Takes a length c and a string, separate the string into multiple strings of length c
separate :: Int -> String -> [String]
separate c = helper []
    where
        helper acc [] = reverse acc
        helper acc s  = helper (take c s:acc) (drop c s)