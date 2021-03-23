main = interact $ show . (\[s, n] -> solve s (read n)) . lines

solve :: String -> Int -> Int
solve s n = (n `div` ls) * nba s + nba (take (n `rem` ls) s)
    where ls  = length s
          nba = foldl (\acc x -> if x=='a' then acc+1 else acc) 0