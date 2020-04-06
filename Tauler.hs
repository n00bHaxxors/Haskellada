module Tauler(
 --casellaBuida,
 --resolt,
 --mou
) where

import Posicio
import Moviment
import Data.Char
import Data.Tuple

data Posicions = [Posicio, Char]

data Tauler = Tau Posicions deriving (Show)


-- ens ha de dir si la posicio que preguntem te casella o esta buida.
--casellaBuida ::
--casellaBuida =

-- ha de fer efectiu un moviment (si es legal) i portar-nos a l'estat que en resulti.
--mou ::
--mou =
