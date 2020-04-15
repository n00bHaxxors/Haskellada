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
} deriving (Eq,Show,Ord)

substituir :: Tauler -> [(Posicio, Char)] -> Tauler
substituir taulerOG newPos = result
    where
        result = Tauler tauFinal (tamany taulerOG)
        tauFinal = map (corregir newPos) (tau taulerOG)
        corregir :: [(Posicio, Char)] -> (Posicio, Char) -> (Posicio, Char)
        corregir posicionsNoves pos = result
            where
                result = if isJust trobat then fromJust trobat else pos
                trobat = find (\x -> fst x == fst pos) posicionsNoves


obtenirStringFila :: Tauler -> Int -> String
obtenirStringFila t y = result
    where
        result = map (casella t) posicions
        posicions = [(Posicio x y) | x <- [0 .. (fst (tamany t) - 1)]]

mostrarTauler :: Tauler -> IO()
mostrarTauler t = do
    let llistaStrings = map (obtenirStringFila t) [0 .. (snd (tamany t) - 1)];
    mapM (putStrLn) llistaStrings;
    putStrLn ""

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