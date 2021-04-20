{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Reader
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)
import Test.QuickCheck

import Life.Cell
import Life.Render
import Life.Simple.Evolve

rowSize :: Int
rowSize = 20

populationSize :: Int
populationSize = 100

cellColor :: Cell -> Color
cellColor Alive = yellow
cellColor Dead = greyN 0.5

renderSettings :: RenderSettings
renderSettings = RenderSettings
  { renderDimension = 20
  , renderPadding = 1
  , renderCellColor = cellColor
  , renderRowSize = rowSize
  }

data SimState = SimState
  { simGenomes :: [Genome]
  , simTargetGrid :: CellGrid
  }

randomGenome :: IO Genome
randomGenome = do
  genomeInit <- replicateM rowSize (generate arbitrary)
  genomeRule <- generate arbitrary
  pure Genome{..}

parseCellGrid :: String -> CellGrid
parseCellGrid = map (map parseCell) . lines
  where
    parseCell :: Char -> Cell
    parseCell '0' = Alive
    parseCell '.' = Dead
    parseCell _ = error "Bad character in target file"

newSimState :: IO SimState
newSimState = do
  genomes <- replicateM populationSize randomGenome
  target <- parseCellGrid <$> readFile "test.target"
  pure (SimState genomes target)

renderSimState :: SimState -> IO Picture
renderSimState (SimState genomes targetGrid) =
  pure . flip runReader renderSettings . fmap pictures $ sequence
    [ renderCellGrid (phenotype bestGenome)
    , renderRule (genomeRule bestGenome)
    , renderInitRow (genomeInit bestGenome)
    ]
  where bestGenome = best targetGrid genomes

handleEvents :: Event -> SimState -> IO SimState
handleEvents (EventKey (SpecialKey KeyEsc) Down _mod _pos) _simState =
  -- When the user presses escape, quit the simulation
  exitSuccess
handleEvents _event simState = pure simState

stepWorld :: Float -> SimState -> IO SimState
stepWorld _time (SimState genomes target) = do
  newGenomes <- evolveStep target genomes
  print (fitness target (best target newGenomes))
  pure (SimState newGenomes target)

main :: IO ()
main = do
  initSimState <- newSimState
  playIO
    FullScreen
    black
    10
    initSimState
    renderSimState
    handleEvents
    stepWorld
