module Posicio (
 Posicio,
 sumar
) where

data Posicio = Posicio {
    x :: Int,
    y :: Int
} deriving (Eq, Ord, Show, Read)

sumar :: Posicio -> (Int, Int) -> Posicio
