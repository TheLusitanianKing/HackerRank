import Control.Monad (replicateM_)

main :: IO ()
main = do
    x <- read <$> getLine
    replicateM_ x (putStrLn "Hello World")