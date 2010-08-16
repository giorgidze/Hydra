module Hydra.Utils.Impossible (impossible) where

import qualified Language.Haskell.TH as TH

impossible :: TH.ExpQ
impossible = do
  loc <- TH.location
  let pos =  (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  let message = "hydra: Impossbile happend at " ++ show pos
  return (TH.AppE (TH.VarE (TH.mkName "error")) (TH.LitE (TH.StringL message)))