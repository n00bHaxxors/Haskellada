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
    result = Bloc x y z (Posicio 0 0)
    x = 1
    y = 1
    -- TODO

crearPosicions :: (Int,String) -> [(Posicio,Char)]
crearPosicions (y,entrada) = result
  where
    result = map (\x -> (Posicio x y, entrada !! x)) [0 .. (length entrada) - 1]


-- [(Posicio, Char)]

crearTauler :: (Int, Int)-> [String] -> Tauler
crearTauler (x,y) entrada = result
  where
    result = Tauler llistaPosicions (x,y)
    llistaY = [0 .. y - 1]
    llistaPosicions = concat (map crearPosicions [(nfila,entrada !! nfila) | nfila <- llistaY])


-- list !! 3 = 1110000000 -> (0,0) .. (10,0)
-- list !! 4 = 1S11110000 -> (0,1) .. (10,1)

sortida :: [String] -> Partida
sortida entrada = result
  where
    result = Partida bloc tauler
    x = read (entrada !! 1) :: Int
    y = read (entrada !! 2) :: Int
    taulerStrings = drop 3 entrada
    tauler = crearTauler (x,y) taulerStrings
    bloc = crearBloc (read (entrada !! 0) :: Int)  taulerStrings

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
  mostrarPartida partida
  putStrLn "Vols que sigui interactiu el joc? S/N"
  interactiu <- getLine
  if interactiu == "S"
  then do
   print (list !! 6) -- mostra la llista 6
  else
   print (list !! 5) -- mostra la llista 5
 else
  putStrLn "El fitxer no existeix"
