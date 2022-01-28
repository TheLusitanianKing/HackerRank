import Control.Monad (replicateM)
import Data.List (sort, nub)
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as M

main :: IO ()
main = do
    [a, b] <- replicateM 2 $ do
        getLine
        words <$> getLine
    putStrLn . unwords . nub . sort $ diff b a

-- could have simply used Data.List.(\\)
-- but then, performances would not be that good
diff :: (Ord a) => [a] -> [a] -> [a]
diff xs ys = filter remains xs
    where
        remains x = M.findWithDefault 0 x oxs > M.findWithDefault 0 x oys
        oxs = occurs xs
        oys = occurs ys

occurs :: (Ord a) => [a] -> Map a Int
occurs = foldr (\x acc -> M.insertWith (+) x 1 acc) M.empty