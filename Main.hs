{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random (randomRIO)

import Life.Cell
import Life.Rule

type Model = [Cells]

step :: Rule -> Cells -> Cells
step rule generation =
  map (apply rule . neighbourhood generation) [0 .. length generation - 1 ]

dimension :: Float
dimension = 20

padding :: Float
padding = 0

cellColor :: Cell -> Color
cellColor Alive = yellow
cellColor Dead = greyN 0.5

renderRule :: Rule -> Picture
renderRule Rule {..} =
  translate (negate $ ruleRenderWidth) 0 $ pictures
    [ renderOption Alive Alive Alive aaa
    , translate (1 * translateUnit) 0 $ renderOption Alive Alive  Dead aad
    , translate (2 * translateUnit) 0 $ renderOption Alive  Dead Alive ada
    , translate (3 * translateUnit) 0 $ renderOption Alive  Dead  Dead add
    , translate (4 * translateUnit) 0 $ renderOption  Dead Alive Alive daa
    , translate (5 * translateUnit) 0 $ renderOption  Dead Alive  Dead dad
    , translate (6 * translateUnit) 0 $ renderOption  Dead  Dead Alive dda
    , translate (7 * translateUnit) 0 $ renderOption  Dead  Dead  Dead ddd
    ]
  where
    translateUnit = 3 * dimension + 2 * padding + 50
    ruleRenderWidth = 4 * (3 * dimension + 2 * padding) + 3 * 50

    renderOption :: Cell -> Cell -> Cell -> Cell -> Picture
    renderOption left middle right result =
      pictures
        [ renderCell left
        , translate (dimension + padding) 0 $ renderCell middle
        , translate (2 * (dimension + padding)) 0 $ renderCell right
        , translate (dimension + padding) (negate $ dimension + padding) $ renderCell result
        ]

renderCell :: Cell -> Picture
renderCell cell = color (cellColor cell) (rectangleSolid dimension dimension)

renderModel :: Model -> Picture
renderModel grid =
  let row n = displayRow (grid !! n)
      translation n = translate 0 (negate $ (fromIntegral n * (dimension + padding)))
  in pictures $ map (\n -> translation n (row n)) [0 .. length grid - 1]
  where
    displayRow :: Cells -> Picture
    displayRow generation =
      let box n = renderCell (generation !! n)
          translation n = translate (fromIntegral n * (dimension + padding)) 0
      in pictures $ map (\n -> translation n (box n)) [0 .. length generation - 1]

simulateRule :: Rule -> IO ()
simulateRule rule =
  simulate
    FullScreen -- display mode
    black      -- background color
    5          -- number of steps to take per second
    initial    -- initial model
    render     -- render the model
    update     -- update the model at each step
  where
    initial:: Model
    initial= [concat (replicate 50 [Dead, Alive, Dead])]

    x = 101 * (dimension + padding) / 2

    render :: Model -> Picture
    render model =
      pictures
        [ translate (-x) 400 (renderModel model)
        , translate 0 500 (renderRule rule)
        ]

    update :: ViewPort -> Float -> Model -> Model
    update _viewPort _time grid =
      let generation = last grid
      in grid <> [step rule generation]

sierpinski :: Rule
sierpinski =
  Rule
    { aaa = Dead
    , aad = Alive
    , ada = Dead
    , add = Alive
    , daa = Alive
    , dad = Dead
    , dda = Alive
    , ddd = Dead
    }

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
  randomCell :: IO Cell
  randomCell = do
    n <- randomRIO (0,1) :: IO Int
    pure (if n == 0 then Dead else Alive)

simulateRandomRule :: IO ()
simulateRandomRule = randomRule >>= simulateRule

main :: IO ()
-- main = simulateRule sierpinski
main = simulateRandomRule
