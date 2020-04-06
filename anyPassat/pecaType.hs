import Data.Char
import Data.Maybe

data Posicio = Posicio { columna :: Char
      , fila :: Int
} deriving (Eq) 

data Peca = Peca { idp :: Char
      , blanca :: Bool
      , posicio :: Posicio
} deriving (Eq) 


instance Show Posicio where
 show (Posicio c f) = [c] ++ show f

instance Show Peca where
 show (Peca i c p) = if c == True
  then [toUpper i]
  else [toLower i]
  


--tellPeca :: Peca -> String 
--tellPeca (Peca {idp = c, posicio = x}) = "Peca: " ++ c ++ show x

--let pos = Posicio {columna = 'A', fila = 2}
--let peo = Peca {idp = "p", posicio = pos}
--tellPeca peo


a1 = Posicio {columna = 'a', fila = 1}
b1 = Posicio {columna = 'b', fila = 1}
c1 = Posicio {columna = 'c', fila = 1}
d1 = Posicio {columna = 'd', fila = 1}
e1 = Posicio {columna = 'e', fila = 1}
f1 = Posicio {columna = 'f', fila = 1}
g1 = Posicio {columna = 'g', fila = 1}
h1 = Posicio {columna = 'h', fila = 1}
a2 = Posicio {columna = 'a', fila = 2}
b2 = Posicio {columna = 'b', fila = 2}
c2 = Posicio {columna = 'c', fila = 2}
d2 = Posicio {columna = 'd', fila = 2}
e2 = Posicio {columna = 'e', fila = 2}
f2 = Posicio {columna = 'f', fila = 2}
g2 = Posicio {columna = 'g', fila = 2}
h2 = Posicio {columna = 'h', fila = 2}
a7 = Posicio {columna = 'a', fila = 7}
b7 = Posicio {columna = 'b', fila = 7}
c7 = Posicio {columna = 'c', fila = 7}
d7 = Posicio {columna = 'd', fila = 7}
e7 = Posicio {columna = 'e', fila = 7}
f7 = Posicio {columna = 'f', fila = 7}
g7 = Posicio {columna = 'g', fila = 7}
h7 = Posicio {columna = 'h', fila = 7}
a8 = Posicio {columna = 'a', fila = 8}
b8 = Posicio {columna = 'b', fila = 8}
c8 = Posicio {columna = 'c', fila = 8}
d8 = Posicio {columna = 'd', fila = 8}
e8 = Posicio {columna = 'e', fila = 8}
f8 = Posicio {columna = 'f', fila = 8}
g8 = Posicio {columna = 'g', fila = 8}
h8 = Posicio {columna = 'h', fila = 8}


ta1b = Peca {idp = 'T', blanca = True, posicio = a1}
cb1b = Peca {idp = 'C', blanca = True, posicio = b1}
ac1b = Peca {idp = 'A', blanca = True, posicio = c1}
dd1b = Peca {idp = 'D', blanca = True, posicio = d1}
re1b = Peca {idp = 'R', blanca = True, posicio = e1}
af1b = Peca {idp = 'A', blanca = True, posicio = f1}
cg1b = Peca {idp = 'C', blanca = True, posicio = g1}
th1b = Peca {idp = 'T', blanca = True, posicio = h1}
pa2b = Peca {idp = 'P', blanca = True, posicio = a2}
pb2b = Peca {idp = 'P', blanca = True, posicio = b2}
pc2b = Peca {idp = 'P', blanca = True, posicio = c2}
pd2b = Peca {idp = 'P', blanca = True, posicio = d2}
pe2b = Peca {idp = 'P', blanca = True, posicio = e2}
pf2b = Peca {idp = 'P', blanca = True, posicio = f2}
pg2b = Peca {idp = 'P', blanca = True, posicio = g2}
ph2b = Peca {idp = 'P', blanca = True, posicio = h2}


ta8b = Peca {idp = 'T', blanca = False, posicio = a8}
cb8n = Peca {idp = 'C', blanca = False, posicio = b8}
ac8n = Peca {idp = 'A', blanca = False, posicio = c8}
dd8n = Peca {idp = 'D', blanca = False, posicio = d8}
re8n = Peca {idp = 'R', blanca = False, posicio = e8}
af8n = Peca {idp = 'A', blanca = False, posicio = f8}
cg8n = Peca {idp = 'C', blanca = False, posicio = g8}
th8n = Peca {idp = 'T', blanca = False, posicio = h8}
pa7n = Peca {idp = 'P', blanca = False, posicio = a7}
pb7n = Peca {idp = 'P', blanca = False, posicio = b7}
pc7n = Peca {idp = 'P', blanca = False, posicio = c7}
pd7n = Peca {idp = 'P', blanca = False, posicio = d7}
pe7n = Peca {idp = 'P', blanca = False, posicio = e7}
pf7n = Peca {idp = 'P', blanca = False, posicio = f7}
pg7n = Peca {idp = 'P', blanca = False, posicio = g7}
ph7n = Peca {idp = 'P', blanca = False, posicio = h7}
--mostrar :: Peca -> String
--mostrar peo= tellPeca peo



--llista de peces amb posicons
data Tauler = Tauler [Peca]
taul = Tauler [ta8b,cb8n,ac8n,dd8n,re8n,af8n,cg8n,th8n,pa7n,pb7n,pc7n,pd7n,pe7n,pf7n,pg7n,ph7n,pa2b,pb2b,pc2b,pd2b,pe2b,pf2b,pg2b,ph2b,ta1b,cb1b,ac1b,dd1b,re1b,af1b,cg1b,th1b]
instance Show Tauler where
 show (Tauler (x:xs)) = show x ++ (show (Tauler xs))



--retorna un maybe peca de la posicio del tauler indicada
buscarPeca :: Tauler -> Posicio -> Maybe Peca
buscarPeca (Tauler t) p = buscarPecaAux t p

--auxiliar de buscarPeca
buscarPecaAux :: [Peca] -> Posicio -> Maybe Peca
buscarPecaAux [] p = Nothing
buscarPecaAux (x:xs) p = if esAqui x p then Just x else buscarPecaAux xs p

--retorna cert si la peca te la posicio indicada
esAqui :: Peca -> Posicio -> Bool
esAqui (Peca id b (Posicio c f)) (Posicio col fil) = (c==col && f==fil)

--passa del valor de la columna de integer a char
intToChar :: Int -> Char
intToChar i
  | i == 1 = 'a'
  | i == 2 = 'b'
  | i == 3 = 'c'
  | i == 4 = 'd'
  | i == 5 = 'e'
  | i == 6 = 'f'
  | i == 7 = 'g'
  | i == 8 = 'h'
  | otherwise = 'z'

--'a'=1, 'b'=2 ... 'h'=8
charToInt :: Char -> Int
charToInt c = (ord c) -96

--Mostra el tauler
mostrarTauler :: Tauler -> String
mostrarTauler  t  = (mostrarTauAux t 10 9)

--Auxiliar per mostrar el tauler
mostrarTauAux :: Tauler -> Int -> Int -> String
mostrarTauAux t i j
  | i > 9 = "    ========"++mostrarTauAux t (i-1) j
  | i <= 9 && (i>=1) = if (j == 9) then  (if i<9 then "|"else "" )++"\n"++ if i>1 then (show (i-1)) ++ "- |"++ mostrarTauAux t (i-1) 1 else mostrarTauAux t (i-1) 1
     else if(x==Nothing) then "." ++ mostrarTauAux t i (j+1) else show (fromJust x) ++ mostrarTauAux t i (j+1) 
  | otherwise = "    ========"++"\n"++"    abcdefgh"
     where x = buscarPeca t (Posicio (intToChar j) i)




data Partida = Partida { tau :: Tauler
 , actual :: Bool
 }
game = Partida {tau = taul, actual = True}
 
--Pe2e4 Pe7e5 --> peo de e2 a e4 i peo de e7 a e5
data Jugada = Jugada { pecaB :: Peca
 , posIniB :: Posicio
 , capturaB :: Bool
 , posFinB :: Posicio
 , escacB :: Bool
 , matB :: Bool
 , pecaN :: Peca
 , posIniN :: Posicio
 , capturaN :: Bool
 , posFinN :: Posicio
 , escacN :: Bool
 , matN :: Bool
 } deriving (Show)

main = putStrLn(mostrarTauler taul)