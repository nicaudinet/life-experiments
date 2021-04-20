{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)
import Control.Monad.Reader (runReader)

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import System.Exit (exitSuccess)
import System.Random (randomIO)

import Life.Cell
import Life.Render
import Life.Simple.Rule

-- * Global Settings

dimension :: Float
dimension = 20

padding :: Float
padding = 0

genSize :: Int
genSize = 20

cellColor :: Cell -> Color
cellColor Alive = yellow
cellColor Dead = greyN 0.5

renderSettings :: RenderSettings
renderSettings = RenderSettings dimension padding cellColor genSize

-- | The whole state of the world
data World = World
  { pastGens :: CellGrid
  , currentGen :: CellRow
  , worldRule :: Rule
  }

-- | The complete state of the simulation
data SimState = SimState
  { simWorld :: World
  , simAdvance :: Bool
  , simFinished :: Bool
  , simViewPort :: ViewPort
  }

-- * Simulate a random thingy

randomWorld :: IO World
randomWorld = do
  rule <- randomRule
  currentGen <- replicateM genSize randomCell
  pure $ World
    { pastGens = []
    , currentGen = currentGen
    , worldRule = rule
    }
  where
    randomCell :: IO Cell
    randomCell = do
      bool <- randomIO 
      pure (if bool then Alive else Dead)

    randomRule :: IO Rule
    randomRule = do
      aaa <- randomCell
      aad <- randomCell
      ada <- randomCell
      add <- randomCell
      daa <- randomCell
      dad <- randomCell
      dda <- randomCell
      ddd <- randomCell
      pure Rule {..}
      where

newSimState :: ViewPort -> IO SimState
newSimState viewPort = do
  world <- randomWorld
  pure $ SimState
    { simWorld = world
    , simAdvance = True
    , simFinished = False
    , simViewPort = viewPort
    }


renderWorld :: RenderSettings -> World -> Picture
renderWorld settings World{..} =
  flip runReader settings . fmap pictures . sequence $
    [ renderRule worldRule
    , renderCellGrid (pastGens <> [currentGen])
    ]

renderSimState :: SimState -> IO Picture
renderSimState = pure . renderWorld renderSettings . simWorld


handleEvents :: Event -> SimState -> IO SimState
handleEvents (EventKey (SpecialKey KeyEnter) Down _mod _pos) SimState{..} =
  -- When the user presses enter, start a new simulation
  newSimState simViewPort
handleEvents (EventKey (SpecialKey KeySpace) Down _mod _pos) simState =
  -- When the user presses space, toggle the advance flag
  pure (simState { simAdvance = not (simAdvance simState)})
handleEvents (EventKey (SpecialKey KeyEsc) Down _mod _pos) _world =
  -- When the user presses escape, quit the simulation
  exitSuccess
handleEvents _ simState = pure simState


stepWorld :: Float -> SimState -> IO SimState
stepWorld _time simState@SimState{..} = do
  if simAdvance && not simFinished
  then pure nextSimState
  else pure simState
  where
    step :: Rule -> CellRow -> CellRow
    step rule cells =
      map (apply rule . neighbourhood cells) [0 .. length cells - 1 ]

    newWorld :: World
    newWorld =
      let World{..} = simWorld
      in World
        { pastGens = pastGens <> [currentGen]
        , currentGen = step worldRule currentGen
        , worldRule = worldRule
        }

    nextSimState :: SimState
    nextSimState = SimState
      { simWorld = newWorld
      , simAdvance = simAdvance
      , simFinished = length (pastGens simWorld) >= genSize
      , simViewPort = simViewPort
      }


main :: IO ()
main = do
  initSimState <- newSimState viewPortInit
  playIO
    FullScreen
    black
    5
    initSimState
    renderSimState
    handleEvents
    stepWorld
