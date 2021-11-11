import Control.Monad (replicateM)
import Data.Array    (Array)

import qualified Data.Array as Array

type Coordinate = (Int, Int)

neighbours :: Coordinate -> Int -> [Coordinate]
neighbours c@(x, y) size = filter inGrid verticalNs ++ filter inGrid horizontalNs
  where
    inGrid = not . outOfGrid
    outOfGrid (a, b) = a < 1 || b < 1 || a > size || b > size
    verticalNs = (,) <$> [x] <*> [y-1, y+1]
    horizontalNs = (,) <$> [x-1, x+1] <*> [y]

breakAt :: Int -> [a] -> [[a]]
breakAt size = go []
  where go acc ls
          | null ls = acc
          | otherwise = go (acc ++ [take size ls]) (drop size ls)

onBorder :: Coordinate -> Int -> Bool
onBorder (x, y) size = x == size || y == size || x == 1 || y == 1

main :: IO ()
main = do
  size <- read <$> getLine
  grid <- Array.listArray ((1, 1), (size, size)) . concat <$> replicateM size getLine
  let
    getValueFromCoordinate c =
      let x = grid Array.! c
          readX = (read :: String -> Int) . (:[]) $ x
          ns = neighbours c size
          nsAreInferior = all (\n -> (read :: String -> Int) [grid Array.! n] < readX) ns
      in 
        if not (onBorder c size) && nsAreInferior then 'X' else x
    cs = map getValueFromCoordinate (Array.indices grid)
  mapM_ putStrLn $ breakAt size cs