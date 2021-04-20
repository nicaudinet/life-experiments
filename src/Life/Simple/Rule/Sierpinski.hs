module Life.Simple.Rule.Sierpinski where

import Life.Cell
import Life.Simple.Rule

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
