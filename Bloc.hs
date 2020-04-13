module Bloc (
  Bloc,
  --mou,
  posBloc
) where

import Posicio
import Moviment

data Bloc = Bloc { x :: Int,
    y :: Int,
    z :: Int,
    Pos :: Posicio
    } deriving (Eq)

-- ha de fer efectiu un moviment (si es legal) i portar-nos a l'estat que en resulti.
--mou :: Bloc -> Moviment -> Bloc
--mou =


-- ens ha de donar les posicions que ocupa el bloc.
posBloc :: Bloc -> Pos
posBloc Bloc = (Pos.posX , Pos.posY)

