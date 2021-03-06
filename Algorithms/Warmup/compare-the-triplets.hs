import Data.List (transpose)
main = interact $ printT . foldl1 sumt . map c . transpose . map (map (read :: String -> Int) . words) . lines
    where c [x, y] = (fromEnum $ x > y, fromEnum $ x < y)
          sumt (x, y) (x', y') = (x + x', y + y')
          printT (x, y) = show x ++ " " ++ show y