module Tauler where

import Posicio
import Moviment
import Bloc
import Data.Char
import Data.Tuple
import Data.Maybe
import Data.List

data Tauler = Tauler {
    tau :: [(Posicio, Char)]
} deriving (Eq, Show)

casella :: Tauler -> Posicio -> Char
casella t p
    | isJust((find (\x -> p == fst x) (tau t))) = snd(fromJust((find (\x -> p == fst x) (tau t))))
    | otherwise = '0'

casellaBuida :: Tauler -> Posicio -> Bool
casellaBuida t p = casella t p == '0'

casellaFi :: Tauler -> Posicio -> Bool
casellaFi t p = casella t p == 'G'

-- caselles: 0 (buida)
--           1 (terra)
--           S (sortida, aqui no ho tindrem)
--           G (goal)
--           B (bloc)