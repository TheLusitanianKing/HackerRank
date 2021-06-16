main :: IO ()
main = interact $ concat . (\[x, y] -> zipWith (\cx cy -> [cx, cy]) x y) . lines