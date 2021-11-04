main :: IO ()
main = do
  [p, d, m, s] <- map read . words <$> getLine
  print $ howManyGames p d m s

howManyGames :: Int -> Int -> Int -> Int -> Int
howManyGames initialPrice discount minPrice = go initialPrice 0
  where
    go currentPrice boughtGames remainingBudget
      | currentPrice > remainingBudget = boughtGames
      | otherwise =
        let
          nextPrice = currentPrice - discount
          price' = if nextPrice < minPrice then minPrice else nextPrice
          budget' = remainingBudget - currentPrice
        in go price' (boughtGames + 1) budget'
