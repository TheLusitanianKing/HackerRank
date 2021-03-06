import Data.List (intercalate)

main = interact $ intercalate "\n" . map show . grading . map read . tail . lines

grading :: [Int] -> [Int]
grading [] = []
grading (x:xs)
    | (nextMult 5 x - x) < 3 && x >= 38
                = nextMult 5 x:grading xs
    | otherwise = x:grading xs

nextMult n x | x `mod` n == 0 = x
             | otherwise      = nextMult n (x+1)