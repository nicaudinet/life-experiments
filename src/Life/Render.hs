{-# LANGUAGE RecordWildCards #-}

module Life.Render where

import Control.Monad.Reader
import Graphics.Gloss

import Life.Types

data RenderSettings = RenderSettings
  { renderDimension :: Float
  , renderPadding :: Float
  , renderCellColor :: Cell -> Color
  , renderRowSize :: Int
  }

type Render a = Reader RenderSettings a

renderWorld :: RenderSettings -> World -> Picture
renderWorld renderSettings World{..} =
  flip runReader renderSettings . fmap pictures . sequence $
    [ renderRule worldRule
    , renderCellGrid (pastGens <> [currentGen])
    ]

renderCell :: Cell -> Render Picture
renderCell cell = do
  cellColor <- asks renderCellColor
  dimension <- asks renderDimension
  pure $ color (cellColor cell) (rectangleSolid dimension dimension)

renderCellOffset :: Int -> Cell -> Render Picture
renderCellOffset offset cell = do
  dimension <- asks renderDimension
  padding <- asks renderPadding
  let x = (dimension + padding) * fromIntegral offset
  translate x 0 <$> renderCell cell

renderCellRow :: CellRow -> Render Picture
renderCellRow = fmap pictures . mapM (uncurry renderCellOffset) . zip [0..]

offsetTranslation :: Int -> Picture -> Picture
offsetTranslation offset = translate x (-5)
  where x = if offset > 9 then -32 else -25

renderOffset :: Int -> Picture
renderOffset n
  = offsetTranslation n
  . scale 0.1 0.1
  . color white
  . text
  . show
  $ n

renderCellRowOffset :: Int -> CellRow -> Render Picture
renderCellRowOffset offset generation = do
  dimension <- asks renderDimension
  padding <- asks renderPadding
  let y = negate $ (dimension + padding) * fromIntegral offset
  fmap (translate 0 y . pictures) . sequence $
    [ pure (renderOffset offset)
    , renderCellRow generation
    ]

renderCellGrid :: CellGrid -> Render Picture
renderCellGrid cellGrid = do
  center <- gridCenter
  translate (-center) center . pictures <$>
    mapM (uncurry renderCellRowOffset) (zip [0..] cellGrid)

gridCenter :: Render Float
gridCenter = do
  a <- boxesWidth
  b <- paddingWidth 
  pure ((a + b) / 2)
  where
    boxesWidth :: Render Float
    boxesWidth = do
      dimension <- asks renderDimension
      rowSize <- asks renderRowSize
      pure (dimension * fromIntegral rowSize)

    paddingWidth :: Render Float
    paddingWidth = do
      padding <- asks renderPadding
      rowSize <- asks renderRowSize
      pure (padding * fromIntegral (rowSize - 1))

renderResult :: Cell -> Render Picture
renderResult = translation <=< renderCell
  where
    translation :: Picture -> Render Picture
    translation picture = do
      dimension <- asks renderDimension
      padding <- asks renderPadding
      let x = dimension + padding
          y = negate $ dimension + padding
      pure (translate x y picture)

renderCase :: (Cell, Cell, Cell, Cell) -> Render Picture
renderCase (left, middle, right, result) =
  fmap pictures $ sequence
    [ renderCell left
    , renderCellOffset 1 middle
    , renderCellOffset 2 right
    , renderResult result
    ]

renderCaseOffset :: Int -> (Cell, Cell, Cell, Cell) -> Render Picture
renderCaseOffset offset ruleCase = do
  dimension <- asks renderDimension
  padding <- asks renderPadding
  let
    column = fromIntegral (offset `mod` 4)
    columnUnit = 4 * dimension + 3 * padding
    row = fromIntegral (offset `div` 4)
    rowUnit = 3 * dimension + 2 * padding
  translate (column * columnUnit) (row * rowUnit) <$> renderCase ruleCase

renderCases :: [(Cell, Cell, Cell, Cell)] -> Render Picture
renderCases ruleCases =
  pictures <$>
    mapM (uncurry renderCaseOffset) (zip [0..] ruleCases)

renderRule :: Rule -> Render Picture
renderRule Rule {..} = do
  dimension <- asks renderDimension
  padding <- asks renderPadding
  center <- gridCenter
  let x = (4 * 3 * dimension + 4 * 2 * padding + 4 * dimension) / 2
      y = center + padding + 8 * dimension
  translate (-x) y <$> renderCases
    [ (Alive, Alive, Alive, aaa)
    , (Alive, Alive,  Dead, aad)
    , (Alive,  Dead, Alive, ada)
    , (Alive,  Dead,  Dead, add)
    , ( Dead, Alive, Alive, daa)
    , ( Dead, Alive,  Dead, dad)
    , ( Dead,  Dead, Alive, dda)
    , ( Dead,  Dead,  Dead, ddd)
    ]

renderInitRow :: CellRow -> Render Picture
renderInitRow initRow = do
  center <- gridCenter
  dimension <- asks renderDimension
  translate (-center) (center + 3 * dimension) <$> renderCellRow initRow
