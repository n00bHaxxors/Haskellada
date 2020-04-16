module Partida where

import Bloc
import Tauler
import Moviment
import Posicio
import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Maybe

data Partida = Partida {
 bloc :: Bloc,
 tauler :: Tauler
} deriving (Eq,Show,Ord)

mostrarPartida :: Partida -> IO()
mostrarPartida p = 
        do
            let taulerAmbBloc = substituir (tauler p) (map (\x -> (x,'B')) (posBloc (bloc p)))
            mostrarTauler taulerAmbBloc

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

    
trobarCami :: Partida -> Map.Map Partida (Partida,Moviment) -> [(Partida,Moviment)] -> [(Partida,Moviment)]
trobarCami partidaActual antecesors cami = do
    let antecesorDelActual = fromJust (Map.lookup partidaActual antecesors)
    let nouTram | fst antecesorDelActual == partidaActual = [(partidaActual,N)]
                | otherwise = (trobarCami (fst antecesorDelActual) antecesors []) ++ [(partidaActual,snd antecesorDelActual)]
    let result = nouTram ++ cami
    result

mostrarPartidaMoviment :: (Int,(Partida,Moviment)) -> IO()
mostrarPartidaMoviment idx_jugada = do
    let jugada = snd idx_jugada
    putStr (if fst idx_jugada == 0 then "" else show(fst idx_jugada) ++ " ")
    mostrarMoviment (snd jugada)
    mostrarPartida (fst jugada)


mostrarCami :: Partida -> Map.Map Partida (Partida,Moviment) -> IO()
mostrarCami p antecesors = do
        let cami = (trobarCami p antecesors [])
        let camiAmbIndexs = zip [0 .. length cami -1] cami
        mapM_ mostrarPartidaMoviment camiAmbIndexs
        putStrLn ("Si, en " ++ show(length camiAmbIndexs) ++ " passos!!!")

--Donat el contingut d un fitxer d entrada, ens retorna una partida a punt de començar
sortida :: [String] -> Partida
sortida entrada = result
  where
    result = Partida bloc tauler
    x = read (entrada !! 2) :: Int
    y = read (entrada !! 1) :: Int
    taulerStrings = drop 3 entrada
    tauler = crearTauler (x,y) taulerStrings
    bloc = crearBloc (read (entrada !! 0) :: Int)  taulerStrings