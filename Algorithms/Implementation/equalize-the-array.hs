import Data.List (group, sortBy)
main = interact $ show . solve . words . last . lines

solve :: [String] -> Int
solve l = length l - mostPresent
    where mostPresent = maximum . map length . group . sortBy (flip compare) $ l