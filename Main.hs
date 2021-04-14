{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM)

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import System.Exit (exitSuccess)
import System.Random (randomIO)

import Life.Rule
import Life.Render.World
import Life.Types

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

--

randomWorld :: ViewPort -> IO World
randomWorld viewPort = do
  rule <- randomRule
  currentGen <- replicateM genSize randomCell
  pure $ World
    { pastGens = []
    , currentGen = currentGen
    , worldRule = rule
    , advance = True
    , finished = False
    , viewPort = viewPort
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


handleEvents :: Event -> World -> IO World
handleEvents (EventKey (SpecialKey KeyEnter) Down _mod _pos) world =
  randomWorld (viewPort world)
handleEvents (EventKey (SpecialKey KeySpace) Down _mod _pos) world =
  pure (world { advance = not (advance world)})
handleEvents (EventKey (SpecialKey KeyEsc) Down _mod _pos) _world = exitSuccess
handleEvents _ world = pure world


stepWorld :: Float -> World -> IO World
stepWorld _time world@World{..} = do
  print (length pastGens)
  if advance && not finished
  then pure newWorld
  else pure world
  where
    step :: Rule -> Generation -> Generation
    step rule cells =
      map (apply rule . neighbourhood cells) [0 .. length cells - 1 ]

    newWorld :: World
    newWorld =
      World
        { pastGens = pastGens <> [currentGen]
        , currentGen = step worldRule currentGen
        , worldRule = worldRule
        , advance = advance
        , finished = length pastGens >= genSize
        , viewPort = viewPort
        }


playRandomRule :: IO ()
playRandomRule = do
  initialWorld <- randomWorld viewPortInit
  playIO
    FullScreen
    black
    5
    initialWorld
    (pure . renderWorld renderSettings)
    handleEvents
    stepWorld


main :: IO ()
main = playRandomRule
