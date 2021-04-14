module Life.Types where

import Graphics.Gloss.Data.ViewPort

-- | Model of a single cell
data Cell = Alive | Dead
  deriving Eq

-- | Model of a row of cells (a generation)
type Generation = [Cell]

-- | An ordered list of past generations
type History = [Generation]

-- | The whole state of the world
data World = World
  { pastGens :: History
  , currentGen :: Generation
  , worldRule :: Rule
  , advance :: Bool
  , finished :: Bool
  , viewPort :: ViewPort
  }

-- | Type that represents a rule
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

-- | A neighbourhood of cells
type Neighbourhood = (Cell, Cell, Cell)

