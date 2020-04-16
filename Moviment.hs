module Moviment where

data Moviment =  U | D | L | R | N deriving (Eq, Show)

mostrarMoviment :: Moviment -> IO()
mostrarMoviment m | m == U = putStrLn "amunt"
                  | m == D = putStrLn "avall"
                  | m == L = putStrLn "esquerra"
                  | m == R = putStrLn "dreta"
                  | otherwise = putStrLn "Inici"