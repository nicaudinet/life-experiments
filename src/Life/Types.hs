module Life.Types where

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


-- | The whole state of the world
data World = World
  { pastGens :: CellGrid
  , currentGen :: CellRow
  , worldRule :: Rule
  }


-- | Type that represents a rule
data Rule = Rule
  { aaa :: Cell
  , aad :: Cell
  , ada :: Cell
  , add :: Cell
  , daa :: Cell
  , dad :: Cell
  , dda :: Cell
  , ddd :: Cell
  }
  deriving Show

instance Arbitrary Rule where
  arbitrary
    =   Rule
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


-- | A neighbourhood of cells
type Neighbourhood = (Cell, Cell, Cell)


-- | A type containing the information (genes) to create a full grid (phenotype)
data Genome = Genome
  { genomeInit :: CellRow
  , genomeRule :: Rule
  }
  deriving Show

instance Arbitrary Genome where
  arbitrary = Genome <$> arbitrary <*> arbitrary 
