module Partida (
 Partida,
 esLegal,
)

import Bloc
import Tauler

data Partida = Partida {
 bloc :: Bloc,
 tauler :: Tauler
} deriving (Eq)

esLegal :: Partida -> Moviment -> Bool

legals :: Partida -> [Moviments]

resolt :: Partida -> Bool
