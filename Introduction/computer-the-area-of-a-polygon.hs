main :: IO ()
main = interact $ show
                . area
                . map (toCoordinate . map read . words)
                . tail
                . lines

type Coordinate = (Int, Int)

toCoordinate :: [Int] -> Coordinate
toCoordinate [x, y] = (x, y)
toCoordinate _      = error "Could not convert to coordinate"

area :: [Coordinate] -> Double
area cs = 0.5 * fromIntegral (sum vs)
    where
        vs = zipWith f cs (tail cs ++ [head cs])
        f (x, y) (x', y') = (x * y') - (x' * y)