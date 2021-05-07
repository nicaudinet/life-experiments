{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Life.Complex.Rule where

import Control.Lens (over)
import Data.Generics.Product
import GHC.Generics
import Test.QuickCheck (Arbitrary, arbitrary)

import Life.Cell

-- | A 2D coordinate
type Coord = (Int, Int)

-- | A cell grid with a focus point
data PointedCellGrid = PointedCellGrid
  { focus :: Coord
  , cellGrid :: CellGrid
  , gridRowSize :: Int
  , gridColSize :: Int
  }
  deriving Generic

getFocus :: PointedCellGrid -> Cell
getFocus (PointedCellGrid (x,y) grid sizeRow sizeCol)
  | x < 0 = Dead
  | y < 0 = Dead
  | x >= sizeRow = Dead
  | y >= sizeCol = Dead
  | otherwise = (grid !! y) !! x

up :: Coord -> Coord
up (x, y) = (x, y + 1)

down :: Coord -> Coord
down (x, y) = (x, y - 1)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

move :: (Coord -> Coord) -> PointedCellGrid -> PointedCellGrid
move = over (field @"focus")

moveBy :: Coord -> PointedCellGrid -> PointedCellGrid
moveBy (x, y) = move (\(a,b) -> (a + x, b + y))

data PatternPiece =
  PatternPiece
    { patternCoord :: Coord
    -- ^ The relative coordinate for the cell we need to match
    , cellState :: Cell
    -- ^ The state the cell needs to be in
    }

instance Arbitrary PatternPiece where
  arbitrary = PatternPiece <$> arbitrary <*> arbitrary

type Pattern = [PatternPiece]

data RulePiece = RulePiece
  { ruleInput :: Pattern
  , ruleOutput :: Cell
  }

instance Arbitrary RulePiece where
  arbitrary = RulePiece <$> arbitrary <*> arbitrary

type Rule = [RulePiece]

matchPiece :: PointedCellGrid -> PatternPiece -> Bool
matchPiece grid (PatternPiece coord state) =
  getFocus (moveBy coord grid) == state

match :: PointedCellGrid -> Pattern -> Bool
match grid = all (matchPiece grid)

-- | Apply a ruleset to a cell in a grid. Use the first rule that matches. If no
-- rules match then kill the cell.
apply :: PointedCellGrid -> Rule -> Cell
apply _grid [] = Dead 
apply grid ((RulePiece pattern output):rs) =
  if match grid pattern
  then output
  else apply grid rs
