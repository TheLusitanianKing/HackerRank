import Control.Monad (forM_, replicateM)
import Data.Sequence (Seq(..))

import qualified Data.Sequence as Seq

rotate :: Int -> Seq a -> Seq a
rotate 0 seq = seq
rotate n (ss :|> s) = rotate (n-1) (s Seq.<| ss)

main :: IO ()
main = do
  [_, k, q] <- map read . words <$> getLine
  ns <- rotate k . Seq.fromList . map (read :: String -> Int) . words <$> getLine
  qs <- map (read :: String -> Int) <$> replicateM q getLine
  mapM_ (print . Seq.index ns) qs