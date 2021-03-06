module Life.Simple.Rule where

import Test.QuickCheck

import Life.Cell

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
  deriving Show

instance Arbitrary Rule where
  arbitrary
    =   Rule
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


-- | A neighbourhood of cells
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

neighbourhood :: CellRow -> Int -> Neighbourhood
neighbourhood generation idx =
  (findCell (idx - 1), findCell idx, findCell (idx + 1))
  where
    findCell :: Int -> Cell
    findCell n
      | n < 0 = Dead
      | n >= length generation = Dead
      | otherwise = generation !! n

