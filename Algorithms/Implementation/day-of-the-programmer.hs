main = interact $ dayOfTheProgrammer . read

dayOfTheProgrammer :: Int -> String
dayOfTheProgrammer year
    | year < 1917 && year `mod` 4 == 0 = "12.09." ++ show year
    | year < 1917 = "13.09." ++ show year
    | year == 1918 = "26.09." ++ show year
    | year > 1918 && (year `mod` 400 == 0 || (year `mod` 4 == 0 && year `mod` 100 /= 0)) = "12.09." ++ show year
    | otherwise = "13.09." ++ show year