main :: IO ()
main = interact $ show . solve . map read . words . last . lines

solve :: [Int] -> Int
solve []     = error "Something is wrong with the input."
solve (f:fs) = go 0 f f 1 fs fs
  where
    go :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> Int
    go areaMaxima alturaMinima altura largura stack elementos
      | null elementos = areaMaxima
      | null stack     =
        let (cabeca:resto) = elementos in go areaMaxima' cabeca cabeca 1 resto resto
      | otherwise      =
        let (cabecaStack:restoStack) = stack
        in go areaMaxima' (max altura alturaMinima) (min altura cabecaStack) (largura + 1) restoStack elementos
      where areaActual  = altura * largura
            areaMaxima' = max areaMaxima areaActual