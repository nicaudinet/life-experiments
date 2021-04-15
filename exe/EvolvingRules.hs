module EvolvingRules where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Life.Types

data SimState

newSimState :: ViewPort -> IO SimState
newSimState viewPort = undefined

initSimState :: SimState
initSimState = undefined

renderSimState :: SimState -> IO Picture
renderSimState = undefined

handleEvents :: Event -> SimState -> IO SimState
handleEvents = undefined

stepWorld :: Float -> SimState -> IO SimState
stepWorld = undefined

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
