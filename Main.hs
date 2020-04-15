module Main where

import System.IO
import System.Directory
import Data.Char
import Data.List
import Tauler
import Partida
import Posicio
import Moviment
import Bloc
import Debug.Trace



--Funcions per crear dades a partir de l entrada
crearBloc :: Int -> [String] -> Bloc
crearBloc z tauler = result
  where
    result = Bloc x y z (Posicio 1 1)
    x = 1
    y = 1
    -- TODO

crearPosicions :: (Int,String) -> [(Posicio,Char)]
crearPosicions (y,entrada) = result
  where
    result = map (\x -> (Posicio x y, if entrada !! x == 'S' then '1' else entrada !! x)) [0 .. (length entrada) - 1]


-- [(Posicio, Char)]

crearTauler :: (Int, Int)-> [String] -> Tauler
crearTauler (x,y) entrada = result
  where
    result = Tauler llistaPosicions (x,y)
    llistaY = [0 .. y - 1]
    llistaPosicions = concat (map crearPosicions [(nfila,entrada !! nfila) | nfila <- llistaY])

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

jocInteractiu :: Partida -> IO()
jocInteractiu p
  | resolt p = putStrLn "Partida resolta"
  | otherwise = do
      putStrLn "Introdueix una direcció [U,D,L,R]"
      entrada <- getLine
      let mov | entrada == "D" = D
              | entrada == "U" = U
              | entrada == "R" = R
              | entrada == "L" = L
              | otherwise = N
      let novaPartida = mou p mov
      mostrarPartida novaPartida
      jocInteractiu novaPartida

solver :: Partida -> IO()
solver p = putStrLn "solvada"

main :: IO ()
main = do
 putStrLn "Quin es el nom del fitxer?"
 nom <- getLine
 exists <- doesFileExist ("./" ++ nom ++ ".txt")
 if exists
 then do
  contents <- readFile (nom ++ ".txt")
  let list = lines contents
  let partida = sortida list
  putStrLn "Vols que sigui interactiu el joc? S/N"
  interactiu <- getLine
  mostrarPartida partida
  if interactiu == "S"
  then do
   jocInteractiu partida
  else
   solver partida
 else
  putStrLn "El fitxer no existeix"
