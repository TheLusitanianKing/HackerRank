import Data.List (sort, sortBy, group, minimumBy)
main = interact $ show
    . fst
    . minimumBy (\(a, la) (b, lb) -> compare lb la)
    . map (\x -> (head x, length x))
    . group
    . sort
    . map (read :: String -> Int)
    . words
    . last
    . lines