{-# LANGUAGE DeriveAnyClass #-}

module Life.Draw where

import Control.Monad.Reader
import Data.Maybe (fromMaybe)
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

posToCoord :: (Float, Float) -> (Int, Int)
posToCoord (x, y) =
  let s = renderDimension renderSettings + renderPadding renderSettings
  in (round (x / s) + 10, (rowSize - (round (y / s) + 10)))

flipCell :: Cell -> Cell
flipCell Dead = Alive
flipCell Alive = Dead

at :: Int -> (a -> a) -> [a] -> [a]
at n fn xs = take n xs <> [fn (xs !! n)] <> drop (n+1) xs

updateGrid :: CellGrid -> (Int, Int) -> CellGrid
updateGrid grid (x, y)
  | x < 0 = grid
  | y < 0 = grid
  | x >= rowSize = grid
  | y >= rowSize = grid
  | otherwise = at y (at x flipCell) grid

handleInputEvents :: Event -> CellGrid -> IO CellGrid
handleInputEvents event grid =
  case event of
    -- Exit the game if the user presses `Esc`
    EventKey (SpecialKey KeyEsc) Down _mod _pos -> exitSuccess
    -- Save the grid to a file if the user presses `s`
    EventKey (Char 's') Down _mod _pos -> do
      writeFile gridFile (showCellGrid grid)
      pure grid
    EventKey (MouseButton LeftButton) Down _mod pos -> do
      print pos
      print (posToCoord pos)
      pure $ updateGrid grid (posToCoord pos)
    _ -> pure grid

displayControllerCallback :: Controller -> IO ()
displayControllerCallback (Controller redraw _) = redraw

draw :: IO ()
draw = do
  let err = error (gridFile <> "failed to parse")
  grid <- fromMaybe err . parseCellGrid <$> readFile gridFile
  interactIO
    FullScreen
    black
    grid
    renderWorld
    handleInputEvents
    displayControllerCallback
