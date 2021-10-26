import Data.Char (isAlpha)

newtype Coordinate = Coordinate
  { coordinateValue :: (Int, Int) } deriving (Eq)

data CellState = Free (Maybe Char) | Blocked

data Cell = Cell
  { cellCoordinate :: Coordinate
  , cellState      :: CellState
  }

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
  , gridCells :: [Cell]
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
    in unlines . map (\y -> concatMap (\x -> displayCell (x, y) (gridCells g)) [0..wi]) $ [0..wi]

allCoordinates :: Int -> [Coordinate]
allCoordinates width = fmap Coordinate $ (,) <$> [0..width'] <*> [0..width']
  where width' = width - 1

-- | From a list of lines representing a grid, read the actual information from it
parseGrid :: [String] -> Grid
parseGrid lines =
  foldr rowFold (Grid width []) $ zip [0..] lines
  where
    width :: Int
    width = length lines 
    rowFold :: (Int, String) -> Grid -> Grid
    rowFold (rowIndex, row) acc = acc <> foldr colFold (Grid width []) (zip [0..] row)
      where
        colFold :: (Int, Char) -> Grid -> Grid
        colFold (columnIndex, col) acc' =
          let c = Coordinate (columnIndex, rowIndex)
              cell = Cell c (parseCellState col)
          in acc' <> Grid width [cell]

-- | Return true if the cell is solved, meaning it is a blocked cell or it has a character
solvedCell :: Cell -> Bool
solvedCell c = case cellState c of
  Blocked       -> True
  Free (Just _) -> True
  _             -> False

-- | Return true if all its cell are solved,
-- it's a naive function as it won't check if the given words are actually there
-- it considers the grid has been completed intelligently
solvedGrid :: Grid -> Bool
solvedGrid = all solvedCell . gridCells

data SegmentOrientation = Down | Right

data Segment = Segment
  { segmentCells :: [Cell]
  , orientation  :: SegmentOrientation
  }

-- | From a grid, determine all its segments
segments :: Grid -> [Segment]
segments = undefined

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