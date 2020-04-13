module Partida where

import Bloc
import Tauler
import Moviment
import Posicio

data Partida = Partida {
 bloc :: Bloc,
 tauler :: Tauler
} deriving (Eq, Show)

--ens ha de tornar els moviments que siguin legals en l'estat" actual.
esLegal :: Partida -> Moviment -> Bool
esLegal p m = not (any (casellaBuida (tauler p)) (posBloc (moure (bloc p) m)))

legals :: Partida -> [Moviment]
legals p = filter (esLegal p) [U,D,L,R]

resolt :: Partida -> Bool
resolt p = all (casellaFi (tauler p)) (posBloc (bloc p))

mou :: Partida -> Moviment -> Partida
mou p m
    | esLegal p m = Partida (moure (bloc p) m) (tauler p)
    | otherwise = p