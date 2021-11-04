import Data.Function (on)
import Data.List     (groupBy, sortBy, tails)

main :: IO ()
main = interact $ show . minDistance . map read . words . last . lines

minDistance :: [Int] -> Int
minDistance xs =
  case gs of
    [] -> -1
    ps -> minimum $ map (minimum . map distance . pairs) ps
  where
    gs = filter ((>1) . length)
      . map (map fst)
      . groupBy ((==) `on` snd)
      . sortBy (compare `on` snd)
      . zip [0..] $ xs

pairs :: [Int] -> [(Int, Int)]
pairs l = [ (x, y) | (x:ys) <- tails l, y <- ys, x /= y ]

distance :: (Int, Int) -> Int
distance (x, y) = abs $ x - y