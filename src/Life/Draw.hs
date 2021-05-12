{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

module Life.Draw where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.IORef
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Life.Cell
import Life.Render
import System.Exit (exitSuccess)

gridFile :: String
gridFile = "grid.txt"

rowSize :: Int
rowSize = 20

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

initCellGrid :: CellGrid
initCellGrid = replicate rowSize (replicate rowSize Dead)

renderWorld :: CellGrid -> IO Picture
renderWorld = pure . flip runReader renderSettings . renderCellGrid

toCoord :: (Float, Float) -> (Int, Int)
toCoord (x, y) =
  let s = renderDimension renderSettings + renderPadding renderSettings
  in (round (x / s) + 10, (rowSize - (round (y / s) + 10)))

flipCell :: Cell -> Cell
flipCell Dead = Alive
flipCell Alive = Dead

at :: Int -> (a -> a) -> [a] -> [a]
at n fn xs = take n xs <> [fn (xs !! n)] <> drop (n+1) xs

updateGrid :: CellGrid -> (Int, Int) -> (Cell -> Cell) -> CellGrid
updateGrid grid (x, y) fn
  | x < 0 = grid
  | y < 0 = grid
  | x >= rowSize = grid
  | y >= rowSize = grid
  | otherwise = at y (at x fn) grid

data MouseState
  = NotPressed
  | Pressed { prevCoord :: (Int, Int), cellType :: Cell }

handleInputEvents :: IORef MouseState -> Event -> CellGrid -> IO CellGrid
handleInputEvents mouseRef event grid = do
  case event of
    -- Exit the game if the user presses `Esc`
    EventKey (SpecialKey KeyEsc) Down _mod _coord -> exitSuccess
    -- Save the grid to a file if the user presses `s`
    EventKey (Char 's') Down _mod _coord -> do
      writeFile gridFile (showCellGrid grid)
      pure grid
    EventKey (MouseButton LeftButton) Down _mod (toCoord -> coord@(x, y)) -> do
      writeIORef mouseRef (Pressed coord (flipCell $ (grid !! x) !! y))
      pure $ updateGrid grid coord flipCell
    EventKey (MouseButton LeftButton) Up _mod _coord -> do
      writeIORef mouseRef NotPressed
      pure grid
    EventMotion (toCoord -> coord) -> do
      mouseState <- readIORef mouseRef
      case mouseState of
        NotPressed -> pure grid
        Pressed{..} ->
          if coord == prevCoord
          then pure grid
          else do
            writeIORef mouseRef (Pressed coord cellType)
            pure $ updateGrid grid coord (const cellType)
    _ -> pure grid

displayControllerCallback :: Controller -> IO ()
displayControllerCallback (Controller redraw _) = redraw

draw :: IO ()
draw = do
  mouseRef <- newIORef NotPressed
  let err = error (gridFile <> "failed to parse")
  grid <- fromMaybe err . parseCellGrid <$> readFile gridFile
  interactIO
    FullScreen
    black
    grid
    renderWorld
    (handleInputEvents mouseRef)
    displayControllerCallback
