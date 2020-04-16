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

--Canvia les posicions coincidents del tauler amb les de la nova llista de posicions
substituir :: Tauler -> [(Posicio, Char)] -> Tauler
substituir taulerOG newPos = result
    where
        result = Tauler tauFinal (tamany taulerOG)
        tauFinal = map (corregir newPos) (tau taulerOG)
        --Donada una llista de pos,char i una nova pos,char substitueix la pos,char de la llista en la mateixa posicio
        corregir :: [(Posicio, Char)] -> (Posicio, Char) -> (Posicio, Char)
        corregir posicionsNoves pos = result
            where
                result = fromMaybe pos trobat
                trobat = find (\x -> fst x == fst pos) posicionsNoves

--Retorna un tauler amb les caselles fantasma del caracter especificat activades
activarInterruptorsLletra :: Tauler -> Char -> Tauler
activarInterruptorsLletra tauler c = do
    let caracter = toLower c
    let posicions = findIndices (\x -> snd x == caracter) (tau tauler)
    substituir tauler (map (\x -> (fst (tau tauler !! x),'1')) posicions)

--Donades unes posicions trepitjades, activa les caselles dels interruptors trepitjats, si escau
activarInterruptors :: Tauler -> [Posicio] -> Tauler
activarInterruptors tauler posicions = do
    let caselles = map (casella tauler) posicions
    let caracters = filter (\x -> isUpper x && x /= 'G' && x /= 'S') caselles
    foldl activarInterruptorsLletra tauler caracters


--retorna una fila específica del tauler en forma de string
obtenirStringFila :: Tauler -> Int -> String
obtenirStringFila t y = result
    where
        result = map (casella t) posicions
        posicions = [Posicio x y | x <- [0 .. (fst (tamany t) - 1)]]


--mostra el tauler per pantalla
mostrarTauler :: Tauler -> IO()
mostrarTauler t = do
    let llistaStrings = map (obtenirStringFila t) [0 .. (snd (tamany t) - 1)];
    mapM_ putStrLn llistaStrings;
    putStrLn ""

--ens dona el caràcter d'una posicio d'un tauler, en cas de posició invalida retorna casella buida
casella :: Tauler -> Posicio -> Char
casella t p
    | isJust(find (\x -> p == fst x) (tau t)) = snd (fromJust (find (\x -> p == fst x) (tau t)))
    | otherwise = '0'

--ens diu si la posicio que preguntem és buida.
casellaBuida :: Tauler -> Posicio -> Bool
casellaBuida t p = casella t p == '0' || isLower (casella t p)

--ens diu si la posicio que preguntem és el destí.
casellaFi :: Tauler -> Posicio -> Bool
casellaFi t p = casella t p == 'G'

--donades unes dimensions X i Y i unes strings del tauler, retorna el tauler descrit
crearTauler :: (Int, Int) -> [String] -> Tauler
crearTauler (x,y) entrada = result
  where
    result = Tauler llistaPosicions (x,y)
    llistaY = [0 .. y - 1]
    llistaPosicions = concatMap crearPosicions ([(nfila, entrada !! nfila) | nfila <- llistaY])

--Crea el vector de posicions segons la string d entrada
crearPosicions :: (Int,String) -> [(Posicio,Char)]
crearPosicions (y,entrada) = result
  where
    result = map (\x -> (Posicio x y, if entrada !! x == 'S' then '1' else entrada !! x)) [0 .. length entrada - 1]

-- caselles: 0 (buida)
--           1 (terra)
--           S (sortida, aqui no ho tindrem)
--           G (goal)
--           B (bloc, aqui tmpoc ho tindrem)