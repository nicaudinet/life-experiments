module Life.Rule.Sierpinski where

import Life.Types

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
