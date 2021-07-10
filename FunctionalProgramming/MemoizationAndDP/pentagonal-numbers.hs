import Control.Monad.State (State, MonadState(state), evalState)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isJust)

main :: IO ()
main = interact $
    unlines . map show . solve . map read . tail . lines

pentagonal :: (Integral a) => a -> State (Map a (a, a)) a
pentagonal x = state (\m -> 
        if x <= 0 then error "Can only work with a natural number"
        else if M.member x m then (fst (m M.! x), m)
        else 
            let mb = M.lookupLT x m
            in if isJust mb
                then let (k, (pts, it)) = fromJust mb in go pts it k m
                else go 0 1 0 m
    )
    where go points iter n m
            | n == x    = (points, m')
            | otherwise = go (points + iter) (iter + 3) (n+1) m'
            where m' = M.insert n (points, iter) m

solve :: (Integral a) => [a] -> [a]
solve xs = evalState (mapM pentagonal xs) M.empty