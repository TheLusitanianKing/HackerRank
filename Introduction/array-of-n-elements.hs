fn :: Int -> String
fn n = show [0..(n-1)]

main :: IO ()
main = do
    n <- readLn :: IO Int
    print $ fn n