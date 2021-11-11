import Data.Char (isUpper)

main :: IO ()
main = interact $ show . (+1) . length . filter isUpper