module Hydra.Stages.Validate (validate) where

import Hydra.Data

validate :: SymTab -> SymTab
validate st =
  let neq = length (equations st)
      nvar = variableNumber st
      msg = "Number of equations (" ++ show neq ++ ") and variables (" ++ show nvar ++ ") do not agree in the model." ++ "\n"
  in if (neq == nvar)
        then st
        else error msg