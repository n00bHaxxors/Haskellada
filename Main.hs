module Main where

import System.IO
import System.Directory
import Data.Char
import Data.List
import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map
import Tauler
import Partida
import Posicio
import Moviment
import Bloc
import Data.Maybe
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

trobarCami :: Partida -> Map.Map Partida Partida -> [Partida] -> [Partida]
trobarCami partidaActual antecesors cami = do
    let antecesorDelActual = fromJust (Map.lookup partidaActual antecesors)
    let nouTram | antecesorDelActual == partidaActual = [partidaActual]
                | otherwise = (trobarCami antecesorDelActual antecesors []) ++ [partidaActual]
    let result = nouTram ++ cami
    result


mostrarCami :: Partida -> Map.Map Partida Partida -> IO()
mostrarCami p antecesors = mapM_ mostrarPartida (trobarCami p antecesors [])

iSolver :: Partida -> Map.Map Partida Partida -> Sequence.Seq (Partida,Moviment)-> IO()
iSolver pActual antecesors pendents = do
  let possiblesMoviments = legals pActual
  let movimentsFiltrats = filter (\x -> Map.notMember (mou pActual x) antecesors) possiblesMoviments
  let nousPendents = pendents Sequence.>< Sequence.fromList [(pActual,m) | m <- movimentsFiltrats]

  let resultat | resolt pActual = mostrarCami pActual antecesors
               | Sequence.null nousPendents = putStrLn "No s'ha trobat solució"
               | otherwise = do
                  let mov = Sequence.index nousPendents 0
                  let novaPartida = mou (fst mov) (snd mov)
                  let nousAntecesors = Map.insert novaPartida (fst mov) antecesors
                  --mostrarPartida novaPartida
                  iSolver novaPartida nousAntecesors (Sequence.drop 1 nousPendents)
  resultat

  --buscar possibles moviments del bloc
  --afegir nous possibles estats (nous) a la sequencia
  --afegir pActual com a antecessor dels nous possibles estats
  --novaPartida = pendents !! 0
  --nousPendents = tail pendents

  --si resolt pActual acabem
  --si no hi ha nous estats possibles i pendents es buit, F

solver :: Partida -> IO()
solver p = do
  let antecessors = Map.insert p p Map.empty
  iSolver p antecessors Sequence.empty


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
  if interactiu == "S"
  then do
   mostrarPartida partida
   jocInteractiu partida
  else
   solver partida
 else
  putStrLn "El fitxer no existeix"
