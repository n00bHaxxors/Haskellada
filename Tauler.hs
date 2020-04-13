module Tauler(
 Tauler,
 casellaBuida
) where

import Posicio
import Moviment
import Data.Char
import Data.Tuple

data Tauler = Tauler {
    tau :: [(Posicio, Char)],
} deriving (Eq)

casellaBuida :: Tauler -> Posicio -> Bool
