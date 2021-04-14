module Life.Rule where

import Life.Types

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

