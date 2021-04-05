{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.MemoTrie (memo, memoFix)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

data Cell = Alive | Dead
  deriving Eq

type Generation = [Cell]

type Grid = [Generation]

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

type Neighbourhood = (Cell, Cell, Cell)

apply :: Rule -> Neighbourhood -> Cell
apply rule (Alive, Alive, Alive) = aaa rule
apply rule (Alive, Alive,  Dead) = aad rule
apply rule (Alive,  Dead, Alive) = ada rule
apply rule (Alive,  Dead,  Dead) = add rule
apply rule ( Dead, Alive, Alive) = daa rule
apply rule ( Dead, Alive,  Dead) = dad rule
apply rule ( Dead,  Dead, Alive) = dda rule
apply rule ( Dead,  Dead,  Dead) = ddd rule

neighbourhood :: Generation -> Int -> Neighbourhood
neighbourhood generation idx =
  (findCell (idx - 1), findCell idx, findCell (idx + 1))
  where
    findCell :: Int -> Cell
    findCell n
      | n < 0 = Dead
      | n >= length generation = Dead
      | otherwise = generation !! n

step :: Rule -> Generation -> Generation
step rule generation =
  map (apply rule . neighbourhood generation) [0 .. length generation - 1 ]

dimension :: Float
dimension = 20

cellColor :: Cell -> Color
cellColor Alive = red
cellColor Dead = blue

displayCell :: Cell -> Picture
displayCell cell = color (cellColor cell) (rectangleSolid dimension dimension)

displayRow :: Generation -> Picture
displayRow generation =
  let box n = displayCell (generation !! n)
      translation n = translate (fromIntegral n * dimension) 0
  in pictures $ map (\n -> translation n (box n)) [0 .. length generation - 1]

displayGrid :: Grid -> Picture
displayGrid grid =
  let row n = displayRow (grid !! n)
      translation n = translate 0 (negate $ fromIntegral n * dimension)
  in
    translate ((fromIntegral $ length (grid !! 0)) /  2) 0 $
      pictures $ map (\n -> translation n (row n)) [0 .. length grid - 1]

simulateRule :: Rule -> IO ()
simulateRule rule =
  simulate
    FullScreen -- display mode
    white -- background color
    5 -- number of steps to take per second
    initialModel
    displayModel
    updateModel
  where
    initialModel :: Grid
    initialModel = [replicate 50 Dead <> [Alive] <> replicate 50 Dead]

    displayModel :: Grid -> Picture
    displayModel = displayGrid

    updateModel :: ViewPort -> Float -> Grid -> Grid
    updateModel _viewPort _time grid =
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

main :: IO ()
main = do
  simulateRule sierpinski
  simulateRule sierpinski
