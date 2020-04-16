module Posicio where

import Data.Tuple

data Posicio = Posicio {
    x :: Int,
    y :: Int
} deriving (Eq, Ord, Show, Read)

--donada una posició i una tupla de dos enters, retorna la posicio resultant d'aplicar la tupla com a desplaçament a la posició
sumar :: Posicio -> (Int, Int) -> Posicio
sumar p s = result
    where
        result = Posicio {x = fst s + x p, y = snd s + y p}

crearPosicions :: (Int,String) -> [(Posicio,Char)]
crearPosicions (y,entrada) = result
  where
    result = map (\x -> (Posicio x y, if entrada !! x == 'S' then '1' else entrada !! x)) [0 .. (length entrada) - 1]