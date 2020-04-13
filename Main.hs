module Main where

import System.IO
import System.Directory
import Data.Char
import Data.List
import Tauler

main :: IO ()
main = do
 putStrLn "Quin es el nom del fitxer?"
 nom <- getLine
 exists <- doesFileExist ("./" ++ nom ++ ".txt")
 if exists
 then do
  contents <- readFile (nom ++ ".txt")
  let list = lines contents
  putStrLn "Vols que sigui interactiu el joc? S/N"
  interactiu <- getLine
  if interactiu == "S"
  then do
   print (list !! 6) -- mostra la llista 6
  else
   print (list !! 5) -- mostra la llista 5
 else
  putStrLn "El fitxer no existeix"
