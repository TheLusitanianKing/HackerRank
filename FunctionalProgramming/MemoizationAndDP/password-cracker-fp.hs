import Control.Monad (replicateM_)
import Data.Foldable (asum)
import Data.List     (isPrefixOf)

crack :: [String] -> String -> String
crack passwords attempt =
    maybe "WRONG PASSWORD" unwords $
        goCrack (filteringPs passwords attempt) attempt []
  where
    filteringPs :: [String] -> String -> [String]
    filteringPs ps atp = filter (`isPrefixOf` atp) ps
    goCrack :: [String] -> String -> [String] -> Maybe [String]
    goCrack fps attempt acc
        | null attempt = Just $ reverse acc
        | null fps     = Nothing
        | otherwise    = 
            let apply mp = goCrack (filteringPs passwords attempt') attempt' (mp:acc)
                    where attempt' = drop (length mp) attempt
            in asum $ map apply fps
            
main :: IO ()
main = do
    t <- read <$> getLine
    replicateM_ t $ do
        getLine
        ps <- words <$> getLine
        attempt <- getLine
        putStrLn $ crack ps attempt