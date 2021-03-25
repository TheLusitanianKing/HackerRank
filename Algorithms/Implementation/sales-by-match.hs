import Data.List (group, sort)

main = interact $ show
    . sum
    . map (\x -> length x `div` 2)
    . group
    . sort
    . map (read :: String -> Int)
    . words
    . last
    . lines