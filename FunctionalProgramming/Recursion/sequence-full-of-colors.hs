import Data.List (tails)

main :: IO ()
main = interact $ unlines . map (show . isFullOfColors) . tail . lines

isFullOfColors :: String -> Bool
isFullOfColors = go (0, 0, 0, 0)
    where
        go (r, g, y, b) [] =
            r == g && y == b
        go colours (x:xs) =
            let colours'@(r',g',y',b') = nextColours colours x
            in abs (r' - g') <= 1 && abs (y' - b') <= 1 && go colours' xs
        nextColours (r, g, y, b) x = case x of
            'R' -> (r+1, g, y, b)
            'G' -> (r, g+1, y, b)
            'Y' -> (r, g, y+1, b)
            'B' -> (r, g, y, b+1)
            _   -> error "Wrong colour given."