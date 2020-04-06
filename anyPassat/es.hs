--main = do
--putStrLn "Entra nom del fitxer"
--nom <- getLine
--x <- readFile "test.txt"
--putStr x

import System.IO
import Data.Char

data Posicio = Posicio { columna :: Char
      , fila :: Int
      } deriving (Eq,Show) 

data Peca = Peca { idp :: Char
      , blanca :: Bool
      , posicio :: Posicio
      } deriving (Eq,Show) 


data Moviment = Moviment {
      peca :: Peca,
      posIni :: Posicio,
      posFi :: Posicio,
      captura :: Bool,
      escac :: Bool,
      mat :: Bool
} deriving (Eq, Show)

data Jugada = Jugada { 
      b :: Moviment,
      n :: Moviment
 } deriving (Show)



main = do  
    handle <- openFile "test.txt" ReadMode  
    contents <- hGetLine handle  
    contents <- hGetLine handle
    putStr contents  
    hClose handle  

readPeca :: Char -> Bool -> Posicio -> Peca
readPeca c b p = Peca c b p

readPos :: String -> Posicio
readPos s = Posicio (head s) (digitToInt ( head ( tail s )))

--Pe5xf4

readMoviment :: String -> Bool -> Moviment
readMoviment s b = result
      where
      result = Moviment peca posIni posFi captura escac mat 
      posIni = readPos (take 2 (drop 1 s))
      peca = readPeca (head (take 1 s)) b posIni
      captura = (take 1 (drop 3 s)) == ['x']
      posFi = if captura then readPos (take 2 (drop 4 s)) else readPos (take 2 (drop 3 s))
      escac = if captura then take 1 (drop 6 s) == ['+'] else take 1 (drop 5 s) == ['+']
      mat = if captura then take 1 (drop 7 s) == ['+'] else take 1 (drop 6 s) == ['+']


readJugada :: String -> Jugada
readJugada s = Jugada(readMoviment((words s)!!0) True)(readMoviment((words s)!!1)False)


--ReadJugada :: String -> Jugada ->Jugada
--ReadJugada [] j = j
--ReadJugada (x:xs) j = j	
--ReadPeca :: String -> Peca













