module Bloc (
 posBloc,
 --esLegal,
 --legals
) where

import Posicio as Pos
import Moviment

type base = Int -- base 
type altura = Int -- altura
type gruix = Int -- gruix

data Estat = Dret | Tombat

-- ens ha de donar les posicions que ocupa el bloc.
posBloc :: (Int, Int)
posBloc = (Pos.posX , Pos.posY)

-- Ens ha de dir si tal com esta el bloc es possible o no fer cert moviment en cert
-- tauler, es a dir, si aquest moviment es legal o fa caure el bloc al buit.
--esLegal :: String -> Bool
--esLegal = True

-- ens ha de tornar els moviments que siguin legals en l'estat actual.
--legals :: [Moviment]
--legals =