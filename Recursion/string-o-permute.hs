main :: IO ()
main = interact $ unlines . map permute . tail . lines

permute :: String -> String
permute xs =
    concatMap (reverse . fst) . filter (\(_, i) -> even i) $ indexedPairs
  where
    indexedPairs = zip pairs [0 ..]
    pairs = zipWith (\a b -> [a, b]) xs (tail xs)