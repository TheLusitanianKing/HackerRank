main = interact $ (\[s, t, k] -> solve s t (read k)) . lines

solve :: String -> String -> Int -> String
solve s t k
    | p == 0 && k >= x + y                     = "Yes"
    | p /= 0 && k >= length s + length t       = "Yes"
    | p /= 0 && even (k - x + y) && k >= x + y = "Yes"
    | otherwise                                = "No"
    where (p, x, y) = removeCommonPrefix s t

-- | Takes two strings, remove their common prefix (if they have one)
-- Returns both the string and the size of the removed prefix (therefore 0 if nothing has been removed)
removeCommonPrefix :: String -> String -> (Int, Int, Int)
removeCommonPrefix s t = let zipped = zip s t in helper 0 s t
    where helper n s t | null s || null t = (n, length s, length t)
          helper n s@(sx:ss) t@(tx:ts)
            | sx == tx         = helper (n+1) ss ts
            | otherwise        = (n, length s, length t)