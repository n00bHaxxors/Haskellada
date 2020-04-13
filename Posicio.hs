module Posicio where

import Data.Tuple

data Posicio = Posicio {
    x :: Int,
    y :: Int
} deriving (Eq, Ord, Show, Read)

sumar :: Posicio -> (Int, Int) -> Posicio
sumar p s = result
    where
        result = Posicio {x = fst s + x p, y = snd s + y p}
