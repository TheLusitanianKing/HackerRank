import Data.Function (on)
import Data.List ((\\), maximumBy, sortBy)
import Text.Printf (printf)

main :: IO ()
main = interact 
    $ printf "%.1f\n"
    . perimeter
    . clockwiseOrder
    . quickHull
    . map (parsePoint . map read . words)
    . tail
    . lines

type Coordinate a = (a, a)
type Point        = Coordinate Int
type Segment      = (Point, Point)

-- | parsing point helper
parsePoint :: [Int] -> Point
parsePoint [x, y] = (x, y)
parsePoint _      = error "Wrong input given for a point."

-- | get the perimeter out of a list of points
perimeter :: [Point] -> Double
perimeter cs = sum ds
    where
        cs' = cs ++ [head cs]
        ds  = zipWith distCoordinate cs' (tail cs')
        distCoordinate (x, y) (x', y') =
            sqrt . fromIntegral $ (x' - x)^2 + (y' - y)^2

-- | returns (left most point, right most point)
leftRightMost:: [Point] -> (Point, Point)
leftRightMost pts = (left, right)
    where sorted = sortBy (compare `on` fst) pts 
          left   = head sorted
          right  = last sorted

data Side = LeftSide | Colinear | RightSide deriving (Eq, Show)

-- | returns the side in which the point is (according to the segment)
whichSide :: Segment -> Point -> Side
whichSide ((xa, ya), (xb, yb)) (xp, yp)
    | crossProduct > 0  = LeftSide
    | crossProduct == 0 = Colinear
    | otherwise         = RightSide
    where crossProduct = ((xb - xa) * (yp - ya)) - ((yb - ya) * (xp - xa))

-- | returns (points to the left of the segment, points to the right of it)
-- and ignore the colinears
sideOfSegment :: Segment -> [Point] -> ([Point], [Point])
sideOfSegment sg pts = (lefts, rights)
    where sides  = map (\pt -> (pt, whichSide sg pt)) pts
          lefts  = map fst . filter ((== LeftSide) . snd) $ sides
          rights = map fst . filter ((== RightSide) . snd) $ sides

-- | returns farthest point from the segment out of the given list of points
farthestPointFromSegment :: Segment -> [Point] -> Point
farthestPointFromSegment sg = maximumBy (compare `on` farthest sg)
    where farthest :: Segment -> Point -> Double
          farthest ((xa, ya), (xb, yb)) (xp, yp) =
              (* 0.5) . abs . fromIntegral $ ((xa - xp) * (yb - ya)) - ((xa - xb) * (yp - ya))

-- | quickhull starting point
quickHull :: [Point] -> [Point]
quickHull pts
    | length pts < 3 = error "Not enough points to get a convex hull."
    | otherwise      =
        [leftmost, rightmost] ++ findHull (leftmost, rightmost) rights ++ findHull (rightmost, leftmost) lefts
  where (leftmost, rightmost) = leftRightMost pts
        remainingPts          = (pts \\ [leftmost]) \\ [rightmost]
        (lefts, rights)       = sideOfSegment (leftmost, rightmost) remainingPts

-- | quickhull helper function
findHull :: Segment -> [Point] -> [Point]
findHull sg@(p, q) pts
    | null pts  = []
    | otherwise =
        let c       = farthestPointFromSegment (p, q) pts
            (s1, _) = sideOfSegment (c, p) pts
            (s2, _) = sideOfSegment (q, c) pts
        in [c] ++ findHull (p, c) s1 ++ findHull (c, q) s2

-- | center (x, y) coordinates from a list of points
center :: [Point] -> Coordinate Double
center pts = (averagex, averagey)
    where 
        nbPoints = length pts
        averagex = (/ fromIntegral nbPoints) . fromIntegral . sum . map fst $ pts
        averagey = (/ fromIntegral nbPoints) . fromIntegral . sum . map snd $ pts

-- | clockwise ordering of a given list of points
clockwiseOrder :: [Point] -> [Point]
clockwiseOrder pts = sortBy (compareF (center pts)) pts
    where
        compareF :: Coordinate Double -> Point -> Point -> Ordering
        compareF (xc, yc) (xa, ya) (xb, yb)
            | xatocenter >= 0 && xbtocenter < 0
                = LT
            | xatocenter < 0 && xbtocenter >= 0
                = GT
            | xatocenter == 0 && xbtocenter == 0 && (yatocenter >= 0 || ybtocenter >= 0)
                = compare yb ya
            | xatocenter == 0 && xbtocenter == 0
                = compare ya yb
            | det < 0
                = LT
            | det > 0
                = GT
            | otherwise
                = compare d2 d1
            where xatocenter = fromIntegral xa - xc
                  xbtocenter = fromIntegral xb - xc
                  yatocenter = fromIntegral ya - yc
                  ybtocenter = fromIntegral yb - yc
                  det = ((fromIntegral xa - xc) * (fromIntegral yb - yc))
                      - ((fromIntegral xb - xc) * (fromIntegral ya - yc))
                  d1 = (fromIntegral xa - xc)^2 + (fromIntegral ya - yc)^2
                  d2 = (fromIntegral xb - xc)^2 + (fromIntegral yb - yc)^2