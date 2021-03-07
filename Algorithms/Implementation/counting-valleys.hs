import Data.List.Split (splitOn)
main = interact $ show
    . length
    . filter (< 0)
    . map head
    . filter (not . null)
    . splitOn [0]
    . scanl (+) 0
    . map (\x -> if x == 'U' then 1 else -1)
    . last
    . lines