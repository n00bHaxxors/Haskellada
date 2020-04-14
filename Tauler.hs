module Tauler where

import Posicio
import Moviment
import Bloc
import Data.Char
import Data.Tuple
import Data.Maybe
import Data.List

data Tauler = Tauler {
    tau :: [(Posicio, Char)],
    tamany :: (Int,Int)
} deriving (Eq,Show)

mostrarTauler :: Tauler -> IO()
mostrarTauler t = putStrLn "tauler"


--instance Show Tauler where
--    show t = show (obtenirSortida t)
--      where
--            obtenirSortida :: Tauler -> [String]
--            obtenirSortida t = result
--               where
--                    result = (map(\fila -> obtenirStringFila t fila) [0 .. snd (tamany t) - 1])
--                    llistaX = [0 .. fst (tamany t) - 1]
--
--           obtenirStringFila :: Tauler -> Int -> String
--          obtenirStringFila t y = "LMAU"
    
    --result = show ["asdf","asdf"] --llistaStrings
    --show t = result
    --    where
    --        llistaX = [0 .. fst (tamany t) - 1]
    --        llistaStrings = ["asdf","asdf"]
    --        llistaStrings = concat (map(\fila -> obtenirStringFila t fila) [0 .. snd (tamany t) - 1])
            
            --concat (map crearPosicions [(nfila,entrada !! nfila) | nfila <- llistaY])

--ens dona el caràcter d'una posicio d'un tauler, en cas de posició invalida retorna casella buida
casella :: Tauler -> Posicio -> Char
casella t p
    | isJust((find (\x -> p == fst x) (tau t))) = snd(fromJust((find (\x -> p == fst x) (tau t))))
    | otherwise = '0'

--ens diu si la posicio que preguntem és buida.
casellaBuida :: Tauler -> Posicio -> Bool
casellaBuida t p = casella t p == '0'

--ens diu si la posicio que preguntem és el destí.
casellaFi :: Tauler -> Posicio -> Bool
casellaFi t p = casella t p == 'G'

-- caselles: 0 (buida)
--           1 (terra)
--           S (sortida, aqui no ho tindrem)
--           G (goal)
--           B (bloc, aqui tmpoc ho tindrem)