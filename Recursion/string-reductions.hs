main :: IO ()
-- if it wasn't for the challenge, I would simply do that
-- main = interact Data.List.nub
main = interact reduce

reduce :: String -> String
reduce = go [] []
    where go acc _ [] = reverse acc
          go acc vs (x:xs) = if x `elem` vs
                             then go acc vs xs
                             else go (x:acc) (x:vs) xs