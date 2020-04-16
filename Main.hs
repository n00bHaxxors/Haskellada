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

--Donada una partida i un índex de partida, interactua amb l'usuari i va avançant el joc, mostrant els passos seguits
jocInteractiu :: (Partida,Int) -> IO()
jocInteractiu (p,i)
  | resolt p = do
      mostrarPartida p
      putStrLn "Partida resolta"
  | otherwise = do
      putStrLn (if i /= 0 then"\nPartida número " ++ show(i) ++"\n"else "")  
      mostrarPartida p
      putStrLn "Introdueix una direcció [U,D,L,R]"
      entrada <- getLine
      let mov | entrada == "D" = D
              | entrada == "U" = U
              | entrada == "R" = R
              | entrada == "L" = L
              | otherwise = N
      let novaPartida = mou p mov
      jocInteractiu (novaPartida,0)

--funcio d'immersió, donada una partida objectiu, un mapa de predecessors i una cua de nodes pendents, mostra el camí per arribar a la partida objectiu
iSolver :: Partida -> Map.Map Partida (Partida,Moviment) -> Sequence.Seq (Partida,Moviment)-> IO()
iSolver pActual antecesors pendents = do
  let possiblesMoviments = legals pActual
  let movimentsFiltrats = filter (\x -> Map.notMember (mou pActual x) antecesors) possiblesMoviments
  let nousPendents = pendents Sequence.>< Sequence.fromList [(pActual,m) | m <- movimentsFiltrats]

  let resultat | resolt pActual = mostrarCami pActual antecesors
               | Sequence.null nousPendents = putStrLn "No hi ha solució !!!"
               | otherwise = do
                  let mov = Sequence.index nousPendents 0
                  let novaPartida = uncurry mou mov
                  let nousAntecesors = Map.insert novaPartida mov antecesors
                  iSolver novaPartida nousAntecesors (Sequence.drop 1 nousPendents)
  resultat

--Donada una partida mostra la seva solució
solver :: Partida -> IO()
solver p = do
  let antecessors = Map.insert p (p,N) Map.empty
  iSolver p antecessors Sequence.empty


main :: IO ()
main = do
 putStrLn "Quin es el nom del fitxer? (sense extensió)"
 nom <- getLine
 exists <- doesFileExist ("./" ++ nom ++ ".txt")
 if exists
 then do
  contents <- readFile (nom ++ ".txt")
  let list = lines contents
  let partides = sortides list
  putStrLn "Vols que sigui interactiu el joc? S/N"
  interactiu <- getLine
  if interactiu == "S"
  then
    mapM_ jocInteractiu (zip partides [1 .. length partides])
  else
   solver (head partides)
 else
  putStrLn "El fitxer no existeix"
