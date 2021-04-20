module Life.Cell where

import Test.QuickCheck

-- | Model of a single cell
data Cell = Alive | Dead
  deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Cell where
  arbitrary = elements [minBound .. maxBound]

showCell :: Cell -> Char
showCell Alive = '0'
showCell Dead = '.'


-- | Model of a row of cells
type CellRow = [Cell]

showCellRow :: CellRow -> String
showCellRow = map showCell


-- | An ordered list of past generations
type CellGrid = [CellRow]

showCellGrid :: CellGrid -> String
showCellGrid = unlines . map showCellRow
