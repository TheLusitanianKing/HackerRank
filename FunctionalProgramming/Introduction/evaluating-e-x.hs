import Text.Printf (printf)

e :: Double -> Double
e x = sum . map (\n -> x^n / fact n) $ [0..9]

fact :: Int -> Double
fact x = fromIntegral $ product [1..x]

pr :: Double -> String
pr = printf "%.4f"

main :: IO ()
main = interact $ unlines . map pr . (\(n:xs) -> map (e . read) xs) . lines