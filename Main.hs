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


iSolver :: Partida -> Map.Map Partida (Partida,Moviment) -> Sequence.Seq (Partida,Moviment)-> IO()
iSolver pActual antecesors pendents = do
  let possiblesMoviments = legals pActual
  let movimentsFiltrats = filter (\x -> Map.notMember (mou pActual x) antecesors) possiblesMoviments
  let nousPendents = pendents Sequence.>< Sequence.fromList [(pActual,m) | m <- movimentsFiltrats]

  let resultat | resolt pActual = mostrarCami pActual antecesors
               | Sequence.null nousPendents = putStrLn "No hi ha solució !!!"
               | otherwise = do
                  let mov = Sequence.index nousPendents 0
                  let novaPartida = mou (fst mov) (snd mov)
                  let nousAntecesors = Map.insert novaPartida mov antecesors
                  iSolver novaPartida nousAntecesors (Sequence.drop 1 nousPendents)
  resultat

solver :: Partida -> IO()
solver p = do
  let antecessors = Map.insert p (p,N) Map.empty
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
