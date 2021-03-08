main = do
    hs <- map read . words <$> getLine
    word <- getLine
    print $ size hs word

letterIndex :: Char -> Int
letterIndex c = fromEnum c - 97

size :: [Int] -> String -> Int
size hs w = height * width
    where height = maximum . map (\c -> hs !! letterIndex c) $ w
          width  = length w