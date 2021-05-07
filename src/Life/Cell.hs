module Life.Cell where

import System.Random (randomIO, randomRIO)
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

-- | Randomly choose one of two cells, with some change of mutation
newCell :: Int -> Cell -> Cell -> IO Cell
newCell mutationChance cell1 cell2 = do
  cond <- randomIO
  let cell = if cond then cell1 else cell2
  mutate <- randomRIO (0, mutationChance)
  if (mutate :: Int) == 0
  then pure $ if cell == Alive then Dead else Alive
  else pure cell
    
