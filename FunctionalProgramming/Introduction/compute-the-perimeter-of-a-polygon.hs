main :: IO ()
main = interact $ show
                . perimeter
                . map (toCoordinate . map read . words)
                . tail
                . lines

type Coordinate = (Int, Int)

toCoordinate :: [Int] -> Coordinate
toCoordinate [x, y] = (x, y)
toCoordinate _      = error "Could not convert to coordinate"

perimeter :: [Coordinate] -> Double
perimeter cs = sum ds
    where
        ds  = zipWith distCoordinate cs' (tail cs')
        cs' = cs ++ [head cs]

distCoordinate :: Coordinate -> Coordinate -> Double
distCoordinate (x, y) (x', y') =
    sqrt . fromIntegral $ (x' - x)^2 + (y' - y)^2