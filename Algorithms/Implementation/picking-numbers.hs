import Data.List (tails)
main = interact $ show
                . tailSeqLength
                . map read
                . words
                . last
                . lines
    
seqLength :: [Int] -> Int -> Int
seqLength [] _ = 0
seqLength l x  = foldl (\acc n -> let diff = x - n in if diff <= 1 && diff >= 0 then acc + 1 else acc) 0 l

tailSeqLength :: [Int] -> Int
tailSeqLength l = maximum . map (seqLength l . head) . filter (not . null) . tails $ l