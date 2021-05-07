module Life.Complex.Evolve where

import Control.Monad (zipWithM, replicateM)
import Data.List (sortOn)
import System.Random (randomIO, randomRIO)

import Life.Cell
import Life.Complex.Rule

data Genome = Genome
  { genomeInit :: CellRow
  , genomeRule :: Rule
  }

type Population = [Genome]

phenotype :: Genome -> CellGrid
phenotype (Genome initRow rule) = iterate step initState !! rowSize
  where
    rowSize :: Int
    rowSize = length initRow - 1

    initState :: CellGrid
    initState = [initRow]

    pointedCellGrid :: CellGrid -> Coord -> PointedCellGrid
    pointedCellGrid grid coord =
      PointedCellGrid
        { focus = coord
        , cellGrid = grid
        , gridRowSize = rowSize
        , gridColSize = length grid
        }

    nextRowCoords :: [Coord]
    nextRowCoords = [ (x,-1) | x <- [0 .. rowSize] ]

    pointedCellGrids :: CellGrid -> [PointedCellGrid]
    pointedCellGrids grid = map (pointedCellGrid grid) nextRowCoords

    step :: CellGrid -> CellGrid
    step grid =
      let row = map (flip apply rule) (pointedCellGrids grid)
      in row : grid

fitness :: CellGrid -> Genome -> Int
fitness targetGrid genome = sum $
  zipWith
    (\a b -> if a == b then 0 else 1)
    (concat $ phenotype genome)
    (concat targetGrid)

mate :: Genome -> Genome -> IO Genome
mate (Genome init1 rule1) (Genome init2 rule2) = Genome <$> newInit <*> newRule
  where
    cell :: Cell -> Cell -> IO Cell
    cell = newCell 50

    newInit :: IO CellRow
    newInit = zipWithM cell init1 init2

    chooseOne :: Int -> Int -> IO Int
    chooseOne a b = do
      cond <- randomIO
      pure (if cond then a else b)

    mateCoord :: Coord -> Coord -> IO Coord
    mateCoord (x1, y1) (x2, y2) = do
      x <- chooseOne x1 x2
      y <- chooseOne y1 y2
      pure (x, y)
      
    matePatternPiece :: PatternPiece -> PatternPiece -> IO PatternPiece
    matePatternPiece (PatternPiece coord1 cell1) (PatternPiece coord2 cell2) =
      PatternPiece <$> mateCoord coord1 coord2 <*> cell cell1 cell2

    matePattern :: Pattern -> Pattern -> IO Pattern
    matePattern = zipWithM matePatternPiece

    mateRulePiece :: RulePiece -> RulePiece -> IO RulePiece
    mateRulePiece (RulePiece in1 out1) (RulePiece in2 out2) =
      RulePiece <$> matePattern in1 in2 <*> cell out1 out2

    newRule :: IO Rule
    newRule = zipWithM mateRulePiece rule1 rule2

evolveStep :: CellGrid -> Population -> IO Population
evolveStep targetGrid genomes = mateGenomes (fitGenomes genomes)
  where
    half :: Int
    half = length genomes `div` 2

    fitGenomes :: [Genome] -> [Genome]
    fitGenomes = take half . sortOn (fitness targetGrid)

    pickOne :: [a] -> IO a
    pickOne xs = do
      idx <- randomRIO (0, half - 1)
      pure (xs !! idx)

    mateGenome :: [Genome] -> IO Genome
    mateGenome parents = do
      mom <- pickOne parents
      dad <- pickOne parents
      mate mom dad
      
    mateGenomes :: [Genome] -> IO [Genome]
    mateGenomes parents = do
      children <- replicateM (length genomes - half) (mateGenome parents)
      pure (parents <> children)

best :: CellGrid -> Population -> Genome
best targetGrid = head . sortOn (fitness targetGrid)
