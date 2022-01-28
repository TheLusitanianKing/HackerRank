import Control.Monad (forM_, replicateM)

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

main :: IO ()
main = do
  n <- read <$> getLine
  ns <- replicateM n $ do
    [l, r] <- map read . words <$> getLine
    return (l, r)
  t <- read <$> getLine
  ks <- replicateM t (read <$> getLine)
  let
    initialTree = createTreeFromInputNodes ns
    swaps = swapsTree initialTree ks
  forM_ swaps $ putStrLn . unwords . map show . traversal 

createTreeFromInputNodes :: [(Int, Int)] -> Tree Int
createTreeFromInputNodes ns = undefined

traversal :: Tree a -> [a]
traversal t = undefined

swapsTree :: Tree a -> [Int] -> [Tree a]
swapsTree = go []
  where
    go acc _ [] = acc
    go acc t (n:ns) =
      let t' = swapTree t n
      in go (acc ++ [t']) t' ns

swapTree :: Tree a -> Int -> Tree a
swapTree t d = undefined