module Moviment where

data Moviment =  U | D | L | R | N deriving (Eq, Show)

mostrarMoviment :: Moviment -> IO()
mostrarMoviment m | m == U = putStrLn "Amunt"
                  | m == D = putStrLn "Avall"
                  | m == L = putStrLn "Esquerra"
                  | m == R = putStrLn "Dreta"
                  | otherwise = putStrLn "Inici"