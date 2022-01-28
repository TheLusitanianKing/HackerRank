import Data.List (inits, tails)

data ShipsPart = LeftPart | CentralPart | RightPart

newtype Assignment = AssignTo (Maybe ShipsPart)

instance Show ShipsPart where
    show LeftPart    = "LEFT"
    show CentralPart = "CENTRAL"
    show RightPart   = "RIGHT"

instance Show Assignment where
    show (AssignTo (Just sp)) = show sp
    show (AssignTo Nothing)   = "DEAD"

isPrime :: (Integral a) => a -> Bool
isPrime k = (k > 1) && null [ x | x <- [2..upperBound], k `mod` x == 0 ]
    where upperBound = floor . sqrt . fromIntegral $ k

assign :: String -> Assignment
assign rawId
    | idIsPrime && hasNoZeros =
        case (leftsArePrimes, rightsArePrimes) of
            (True, True)  -> AssignTo $ Just CentralPart
            (True, False) -> AssignTo $ Just LeftPart
            (False, True) -> AssignTo $ Just RightPart
            _             -> AssignTo Nothing
    | otherwise  = AssignTo Nothing
    where idIsPrime = isPrime . read $ rawId
          hasNoZeros = '0' `notElem` rawId
          leftsArePrimes  = all (isPrime . read) . init . tails $ rawId
          rightsArePrimes = all (isPrime . read) . tail . inits $ rawId

main :: IO ()
main = interact $ unlines . map (show . assign) . tail . lines