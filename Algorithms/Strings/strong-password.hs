import Data.Char (isDigit, isLower, isUpper)

main :: IO ()
main = interact $ show . solve . last . lines

specials :: [Char]
specials = "!@#$%^&*()-+"

solve :: String -> Int
solve s = minToAdd + toAddToComplete
  where
    missingLength = 6 - length s
    hasDigit = any isDigit s
    hasUpper = any isUpper s
    hasLower = any isLower s
    hasSpecial = any (`elem` specials) s
    oneToAdd p = if p then 0 else 1
    minToAdd = sum $ map oneToAdd [hasDigit, hasUpper, hasLower, hasSpecial]
    remainingAfterAdd = missingLength - minToAdd
    toAddToComplete = if remainingAfterAdd > 0 then remainingAfterAdd else 0