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
parseCellState _   = error "Unknown cell state"

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

solve :: Grid -> [String] -> Grid
solve grid words = grid -- TODO

main :: IO ()
main = interact $
  show . (\ls -> solve (parseGrid $ init ls) (words . last $ ls)) . lines