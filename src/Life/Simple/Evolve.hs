{-# LANGUAGE RecordWildCards #-}

module Life.Simple.Evolve where

import Control.Monad (zipWithM, replicateM)
import Data.List (sortOn)
import System.Random (randomIO, randomRIO)

import Life.Types
import Life.Rule

phenotype :: Genome -> CellGrid
phenotype (Genome initRow rule) = snd (iterate step initState !! genSize)
  where
    genSize :: Int
    genSize = length initRow

    initState :: (CellRow, CellGrid)
    initState = (initRow, [])

    applyRule :: CellRow -> CellRow
    applyRule cellRow =
      map (apply rule . neighbourhood cellRow) [0 .. genSize - 1] 

    step :: (CellRow, CellGrid) -> (CellRow, CellGrid)
    step (currentRow, grid) = (applyRule currentRow, grid <> [currentRow])

fitness :: CellGrid -> Genome -> Int
fitness targetGrid genome = sum $
  zipWith
    (\a b -> if a == b then 0 else 1)
    (concat $ phenotype genome)
    (concat targetGrid)

mate :: Genome -> Genome -> IO Genome
mate (Genome init1 rule1) (Genome init2 rule2) = Genome <$> newInit <*> newRule
  where
    newCell :: Cell -> Cell -> IO Cell
    newCell cell1 cell2 = do
      cond <- randomIO
      let cell = if cond then cell1 else cell2
      mutate <- randomRIO (0, 100)
      if (mutate :: Int) == 0
      then pure $ if cell == Alive then Dead else Alive
      else pure cell
    
    newInit :: IO CellRow
    newInit = zipWithM newCell init1 init2

    newRule :: IO Rule
    newRule = do
      aaa <- newCell (aaa rule1) (aaa rule2)
      aad <- newCell (aad rule1) (aad rule2)
      ada <- newCell (ada rule1) (ada rule2)
      add <- newCell (add rule1) (add rule2)
      daa <- newCell (daa rule1) (daa rule2)
      dad <- newCell (dad rule1) (dad rule2)
      dda <- newCell (dda rule1) (dda rule2)
      ddd <- newCell (ddd rule1) (ddd rule2)
      pure Rule{..}

evolveStep :: CellGrid -> [Genome] -> IO [Genome]
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

best :: CellGrid -> [Genome] -> Genome
best targetGrid = head . sortOn (fitness targetGrid)
