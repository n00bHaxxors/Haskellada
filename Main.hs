module Main where

import System.IO
import System.Directory
import Data.Char

-- ens ha de crear un tauler i bloc a l'estat inicial.
--sortida ::
--sortida = 

-- ens ha de dir si el bloc ja ha arribat a la meta o no.
--resolt :: Bool
--resolt = True

main :: IO ()
main = do
 putStrLn "Quin es el nom del fitxer?"
 nom <- getLine
 exists <- doesFileExist ("./" ++ nom ++ ".txt")
 if exists
 then do
  fitxer <- openFile (nom ++ ".txt") ReadMode
  contents <- hGetContents fitxer
  putStrLn contents
  hClose fitxer
 else
  putStrLn "El fitxer no existeix"