import Data.Char  (isAlpha)
import Data.List  (nub)
import Data.Maybe (mapMaybe)
import Data.Set   (Set)

import qualified Data.Set as Set

newtype Coordinate = Coordinate
  { coordinateValue :: (Int, Int) } deriving (Eq, Ord)

instance Show Coordinate where
  show = show . coordinateValue

data CellState = Free (Maybe Char) | Blocked deriving (Eq, Ord)

data Cell = Cell
  { cellCoordinate :: Coordinate
  , cellState      :: CellState
  } deriving (Eq, Ord, Show)

instance Show CellState where
  show Blocked         = "+"
  show (Free Nothing)  = "-"
  show (Free (Just c)) = [c]

-- TODO: make CellState an instance of Read instead?
parseCellState :: Char -> CellState
parseCellState '-' = Free Nothing
parseCellState '+' = Blocked
parseCellState c
  | isAlpha c = Free (Just c)
  | otherwise = error "Unknown cell state"

data Grid = Grid
  { gridWidth :: Int
  , gridCells :: Set Cell
  }

instance Semigroup Grid where
  (Grid w1 c1) <> (Grid w2 c2)
    | w1 /= w2  = error ""
    | otherwise = Grid w1 $ c1 <> c2

instance Show Grid where
  show g =
    let wi = gridWidth g - 1
        displayCell (x, y) cells =
          case filter (\cell -> cellCoordinate cell == Coordinate (x, y)) cells of
            [val] -> show $ cellState val
            _     -> error ""
    in unlines . map (\y -> concatMap (\x -> displayCell (x, y) (Set.toList $ gridCells g)) [0..wi]) $ [0..wi]

allCoordinates :: Int -> [Coordinate]
allCoordinates width = fmap Coordinate $ (,) <$> [0..width'] <*> [0..width']
  where width' = width - 1

-- | From a list of lines representing a grid, read the actual information from it
parseGrid :: [String] -> Grid
parseGrid lines =
  foldr rowFold (Grid width Set.empty) $ zip [0..] lines
  where
    width :: Int
    width = length lines 
    rowFold :: (Int, String) -> Grid -> Grid
    rowFold (rowIndex, row) acc = acc <> foldr colFold (Grid width Set.empty) (zip [0..] row)
      where
        colFold :: (Int, Char) -> Grid -> Grid
        colFold (columnIndex, col) acc' =
          let c = Coordinate (columnIndex, rowIndex)
              cell = Cell c (parseCellState col)
          in acc' <> Grid width (Set.fromList [cell])

-- | Return true if the cell is solved, meaning it is a blocked cell or it has a character
solvedCell :: Cell -> Bool
solvedCell c =
  case cellState c of
    Blocked       -> True
    Free (Just _) -> True
    _             -> False

-- | Return true if the cell is free to be completed (or already completed)
freeCell :: Cell -> Bool
freeCell c = 
  case cellState c of
    Free _ -> True
    _      -> False

-- | Return true if all its cell are solved,
-- it's a naive function as it won't check if the given words are actually there
-- it considers the grid has been completed intelligently
solvedGrid :: Grid -> Bool
solvedGrid = all solvedCell . gridCells

data SegmentOrientation = Vertical | Horizontal deriving (Eq, Show)

data Segment = Segment
  { segmentCells       :: Set Cell
  , segmentOrientation :: SegmentOrientation
  } deriving (Eq, Show)

-- | From a grid, determine all its segments
segments :: Grid -> [Segment]
segments g =
  let w = gridWidth g
      freeCells = Set.toList . Set.filter freeCell $ gridCells g
      getSegments :: Cell -> [Segment]
      getSegments c = mapMaybe (\c' -> trySegment c c' freeCells) freeCells
      trySegment :: Cell -> Cell -> [Cell] -> Maybe Segment
      trySegment c1 c2 allFreeCells
        | x1 == x2 && abs (y1 - y2) == 1 =
          let coordinates = fmap Coordinate $ (,) <$> [x1] <*> [0..w]
          in return Segment
              { segmentOrientation = Vertical
              , segmentCells = Set.fromList $ filter ((`elem` coordinates). cellCoordinate) freeCells
              }
        | y1 == y2 && abs (x1 - x2) == 1 =
          let coordinates = fmap Coordinate $ (,) <$> [0..w] <*> [y1]
          in return Segment
            { segmentOrientation = Horizontal
            , segmentCells = Set.fromList $ filter ((`elem` coordinates). cellCoordinate) freeCells
            }
        | otherwise = Nothing
        where (x1, y1) = coordinateValue . cellCoordinate $ c1
              (x2, y2) = coordinateValue . cellCoordinate $ c2
  in nub $ concatMap getSegments freeCells

-- | Apply a string to a segment, will return a segment on success
applyWordToSegment :: Segment -> String -> Maybe Segment
applyWordToSegment = undefined

newtype CollapseNode = CollapseNode { collapsingCells :: (Cell, Cell) }

-- | From a list of segments, will return all the collapsing nodes
collapsingSegments :: [Segment] -> [CollapseNode]
collapsingSegments = undefined

-- | Determines if all the collapses are OK
collapsesWell :: [CollapseNode] -> Bool
collapsesWell = undefined

-- | Apply segments to a grid, will return a new grid with the applied segments on success
applySegments :: Grid -> [Segment] -> Maybe Grid
applySegments = undefined

-- | From non-completed grid and a list of words, will return a soluce list of grid
-- or an error if it can't find a solution, which shouldn't happen
solve :: Grid -> [String] -> Grid
solve grid words = undefined

main :: IO ()
main = interact $
  show . (\ls -> solve (parseGrid $ init ls) (words . last $ ls)) . lines