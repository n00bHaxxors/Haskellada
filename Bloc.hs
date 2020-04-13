module Bloc (
  Bloc,
  aplicarMoviment,
  PposicionsOcupades
) where

import Posicio as Pos
import Moviment

data Bloc = Bloc { x :: Int,
    y :: Int,
    z :: Int,
    pos :: Pos
    } deriving (Eq)

-- ha de fer efectiu un moviment (si es legal) i portar-nos a l'estat que en resulti.
mou :: Bloc -> Moviment -> Bloc
mou =


-- ens ha de donar les posicions que ocupa el bloc.
posBloc :: Bloc -> Pos
posBloc = (Pos.posX , Pos.posY)
