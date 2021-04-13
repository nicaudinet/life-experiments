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

import Life.Cell
import Life.Rule

type Model = [Cells]

dimension :: Float
dimension = 20

padding :: Float
padding = 0

genSize :: Int
genSize = 20

cellColor :: Cell -> Color
cellColor Alive = yellow
cellColor Dead = greyN 0.5

data World = World
  { pastGens :: [Cells]
  , currentGen :: Cells
  , worldRule :: Rule
  , advance :: Bool
  , finished :: Bool
  , viewPort :: ViewPort
  }


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


renderWorld :: World -> Picture
renderWorld World {..} =
  pictures
    [ translate (-400) 400 (renderRule worldRule)
    , translate (-400) 300 (renderHistory (pastGens <> [currentGen]))
    ]
  where
    renderCell :: Cell -> Picture
    renderCell cell =
      color (cellColor cell) (rectangleSolid dimension dimension)

    renderCellOffset :: Int -> Cell -> Picture
    renderCellOffset offset =
      translate ((dimension + padding) * fromIntegral offset) 0 . renderCell

    renderGen :: Cells -> Picture
    renderGen = pictures . map (uncurry renderCellOffset) . zip [0..]

    renderGenOffset :: Int -> Cells -> Picture
    renderGenOffset offset =
      translate 0 (negate $ dimension * fromIntegral offset) . renderGen

    renderHistory :: [Cells] -> Picture
    renderHistory = pictures . map (uncurry renderGenOffset) . zip [0..]

    renderResult :: Cell -> Picture
    renderResult = translation . renderCell
      where
        translation :: Picture -> Picture
        translation = 
          translate (dimension + padding) (negate $ dimension + padding)

    renderCase :: (Cell, Cell, Cell, Cell) -> Picture
    renderCase (left, middle, right, result) =
      pictures
        [ renderCell left
        , renderCellOffset 1 middle
        , renderCellOffset 2 right
        , renderResult result
        ]

    renderCaseOffset :: Int -> (Cell, Cell, Cell, Cell) -> Picture
    renderCaseOffset offset =
      let
        column = fromIntegral (offset `mod` 4)
        columnUnit = 4 * dimension + 3 * padding
        row = fromIntegral (offset `div` 4)
        rowUnit = 3 * dimension + 2 * padding
      in
        translate (column * columnUnit) (row * rowUnit) . renderCase

    renderCases :: [(Cell, Cell, Cell, Cell)] -> Picture
    renderCases = pictures . map (uncurry renderCaseOffset) . zip [0..]
    
    renderRule :: Rule -> Picture
    renderRule Rule {..} =
      renderCases $
        [ (Alive, Alive, Alive, aaa)
        , (Alive, Alive,  Dead, aad)
        , (Alive,  Dead, Alive, ada)
        , (Alive,  Dead,  Dead, add)
        , ( Dead, Alive, Alive, daa)
        , ( Dead, Alive,  Dead, dad)
        , ( Dead,  Dead, Alive, dda)
        , ( Dead,  Dead,  Dead, ddd)
        ]


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
    step :: Rule -> Cells -> Cells
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
    (pure . renderWorld)
    handleEvents
    stepWorld


main :: IO ()
main = playRandomRule
