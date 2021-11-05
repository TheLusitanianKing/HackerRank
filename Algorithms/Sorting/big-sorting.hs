import Data.List (sort)

main :: IO ()
main = interact $
  unlines . map show . sort . map (read :: String -> Integer) . tail . lines