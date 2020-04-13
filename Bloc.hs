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

aplicarMoviment :: Bloc -> Moviment -> Bloc

posicionsOcupades :: Bloc -> [Posicio]
