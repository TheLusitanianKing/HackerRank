import Data.List (group)

main :: IO ()
main = interact $
    concatMap (\x -> if length x > 1 then head x : show (length x) else x) . group