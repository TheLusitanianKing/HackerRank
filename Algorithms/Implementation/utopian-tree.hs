utopian = scanl1 (\acc x -> if odd x then acc + 1 else acc * 2) [1..]
main = interact $ unlines . map (show . (!!) utopian . read) . tail . lines