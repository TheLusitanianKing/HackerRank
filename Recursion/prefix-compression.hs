main :: IO ()
main = interact $ unlines
                . map (\x -> show (length x) ++ " " ++ x)
                . (\[x, y] -> compress x y)
                . lines

-- | From a string x and a string y, returns their common prefix
--   and both remaining string after stripped from the common prefix
compress :: String -> String -> [String]
compress x y = [p, x', y']
    where
        p  = map fst . takeWhile (uncurry (==)) $ zip x y
        x' = drop (length p) x
        y' = drop (length p) y