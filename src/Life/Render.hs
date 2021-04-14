{-# LANGUAGE RecordWildCards #-}

module Life.Render
  ( RenderSettings(..)
  , renderWorld
  ) where

import Graphics.Gloss

import Life.Types

data RenderSettings = RenderSettings
  { renderDimension :: Float
  , renderPadding :: Float
  , renderCellColor :: Cell -> Color
  , renderGenSize :: Int
  }

renderWorld :: RenderSettings -> World -> Picture
renderWorld (RenderSettings dimension padding cellColor genSize) World{..} =
  pictures
    [ renderRule worldRule
    , renderHistory (pastGens <> [currentGen])
    ]
  where
    renderCell :: Cell -> Picture
    renderCell cell =
      color (cellColor cell) (rectangleSolid dimension dimension)

    renderCellOffset :: Int -> Cell -> Picture
    renderCellOffset offset =
      translate ((dimension + padding) * fromIntegral offset) 0 . renderCell

    renderGeneration :: Generation -> Picture
    renderGeneration = pictures . map (uncurry renderCellOffset) . zip [0..]

    renderGenerationOffset :: Int -> Generation -> Picture
    renderGenerationOffset offset =
      translate 0 (negate $ dimension * fromIntegral offset) . renderGeneration

    renderHistory :: [Generation] -> Picture
    renderHistory
      = translate (-historyCenter) historyCenter
      . pictures
      . map (uncurry renderGenerationOffset)
      . zip [0..]

    historyCenter :: Float
    historyCenter = (boxesWidth + paddingWidth) / 2
      where
        boxesWidth :: Float
        boxesWidth = dimension * fromIntegral genSize

        paddingWidth :: Float
        paddingWidth = padding * fromIntegral (genSize - 1)

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
      translate (-x) y . renderCases $
        [ (Alive, Alive, Alive, aaa)
        , (Alive, Alive,  Dead, aad)
        , (Alive,  Dead, Alive, ada)
        , (Alive,  Dead,  Dead, add)
        , ( Dead, Alive, Alive, daa)
        , ( Dead, Alive,  Dead, dad)
        , ( Dead,  Dead, Alive, dda)
        , ( Dead,  Dead,  Dead, ddd)
        ]
      where
        x :: Float
        x = (4 * 3 * dimension + 4 * 2 * padding + 4 * dimension) / 2

        y :: Float
        y = historyCenter + padding + 6 * dimension


