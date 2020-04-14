module Partida where

import Bloc
import Tauler
import Moviment
import Posicio

data Partida = Partida {
 bloc :: Bloc,
 tauler :: Tauler
} deriving (Eq,Show)

mostrarPartida :: Partida -> IO()
mostrarPartida p = 
        do
            mostrarBloc (bloc p)
            mostrarTauler (tauler p)

--retorna si un moment és legal amb l'estat actual.
esLegal :: Partida -> Moviment -> Bool
esLegal p m = not (any (casellaBuida (tauler p)) (posBloc (moure (bloc p) m)))

--retorna un llistat dels moviments legals en l'estat actual.
legals :: Partida -> [Moviment]
legals p = filter (esLegal p) [U,D,L,R]

--diu si el bloc ja ha arribat a la meta o no.
resolt :: Partida -> Bool
resolt p = all (casellaFi (tauler p)) (posBloc (bloc p))

--efectua un moviment si aquest és legal
mou :: Partida -> Moviment -> Partida
mou p m
    | esLegal p m = Partida (moure (bloc p) m) (tauler p)
    | otherwise = p