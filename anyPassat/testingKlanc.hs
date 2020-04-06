
import System.IO
import Data.Char
import Data.Maybe
import Data.List

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


ta8n = Peca {idp = 'T', blanca = False, posicio = a8}
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

taul = Tauler [ta8n,cb8n,ac8n,dd8n,re8n,af8n,cg8n,th8n,pa7n,pb7n,pc7n,pd7n,pe7n,pf7n,pg7n,ph7n,pa2b,pb2b,pc2b,pd2b,pe2b,pf2b,pg2b,ph2b,ta1b,cb1b,ac1b,dd1b,re1b,af1b,cg1b,th1b]
movTest = Moviment {peca = pe2b, posIni= e2,posFi = Posicio {columna = 'e', fila = 4},captura=False, escac=False, mat=False}

------ TIPUS

data Posicio = Posicio { columna :: Char
      , fila :: Int
      } deriving (Eq) 

data Peca = Peca { idp :: Char
      , blanca :: Bool
      , posicio :: Posicio
      } deriving (Eq) 


data Moviment = Moviment {
      peca :: Peca,
      posIni :: Posicio,
      posFi :: Posicio,
      captura :: Bool,
      escac :: Bool,
      mat :: Bool
} deriving (Eq, Show)

data Jugada = Jugada { 
      b :: Moviment,
      n :: Maybe Moviment
 } deriving (Show)

data Tauler = Tauler [Peca]
 
data Partida = Partida { tau :: Tauler
 , actual :: Bool
 }
game = Partida {tau = taul, actual = True}

------ INSTANCES

instance Show Posicio where
 show (Posicio c f) = [c] ++ show f
   
instance Show Peca where
 show (Peca i c p) = if c == True
     then [toUpper i]
     else [toLower i]

instance Show Tauler where
  show (Tauler (x:xs)) = show x ++ (show (Tauler xs))

--------- Checking Jugada

{-
checkJugada :: handle -> Partida -> (handle,(Bool, Partida))
checkJugada h p = do
    if (hIsEOF h) then return (h,(True,p))
    else
        contents <- hGetLine h  
        jugada <- readJugada contents
        if esLegal jugada p then do
           p <- aplicarJugada jugada p
           return (checkJugada h p)
        else
            return (h,(False,p))
-}



aplicarMoviment :: Moviment -> Partida -> Partida
aplicarMoviment m p = p

checkJugada :: [String] -> Partida -> (([String],[String]),(Bool, Partida))
checkJugada s p = result
    where 
      result
        | null s = ((s, []), (True, p))
        | checkMoviment p (b jugada) =
          if isNothing (n jugada) then ((s,s), (True, p)) else
            if checkMoviment p (fromJust (n jugada)) then checkJugada (tail s) j else
              ((s,s), (False, p))
        | otherwise = ((s,s), (False, p)) 
      jugada = readJugada contents
      contents = head s
      j =  aplicarMoviment (fromJust (n jugada)) p
      p = aplicarMoviment (b jugada) p


checkJugades :: [String] -> Partida -> (Bool,Partida) --Donat el nom d una partida, i  una Partida, retorna el resultat de la partida, i si la partida és vàlida o no
checkJugades s p = result
    where
      result = snd (checkJugada s p)


-----------------------------------------------MAIN------
main :: IO ()
main = do
  content <- readFile "test.txt"
  let linies = lines content
  let result = fst (checkJugades linies game)
  print result

-----------------------------------------------Entrada fitxer------
readPeca :: Char -> Bool -> Posicio -> Peca
readPeca c b p = Peca c b p

readPos :: String -> Posicio
readPos s = Posicio (head s) (digitToInt ( head ( tail s )))

--Pe5xf4

readMoviment :: String -> Bool -> Moviment
readMoviment s b = result
      where
      result = Moviment peca posIni posFi captura escac mat 
      posIni = readPos (take 2 (drop 1 s))
      peca = readPeca (head (take 1 s)) b posIni
      captura = take 1 (drop 3 s) == ['x']
      posFi = if captura then readPos (take 2 (drop 4 s)) else readPos (take 2 (drop 3 s))
      escac = if captura then take 1 (drop 6 s) == ['+'] else take 1 (drop 5 s) == ['+']
      mat = if captura then take 1 (drop 7 s) == ['+'] else take 1 (drop 6 s) == ['+']


readJugada :: String -> Jugada
readJugada s = 
    if length (words s) == 2 then Jugada(readMoviment(head (words s)) True) (Just (readMoviment(head (tail (words s)))False))
    else Jugada(readMoviment s True) Nothing

-------------------------------------------------
-----------------------------------------------Mostrar tauler------


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


-----------------------------------------------Funcions de moviments------

--canvia de posicio una peca
mourePeca :: Tauler -> Moviment -> Tauler
mourePeca (Tauler t) (Moviment (Peca i b pos) pi pf c e m) = Tauler ((delete pecaAMoure t)++[Peca i b pf])
 where
  pecaAMoure = fromJust(buscarPeca (Tauler t) pi)
  --pMoure = if pecaAMoure == Nothing then (Peca i b pos) else fromJust pMoure

--borrarElement :: Peca -> Tauler -> Tauler
--borrarElement x (Tauler p) = Tauler (delete x p)

checkMoviment :: Partida -> Moviment -> Bool
checkMoviment (Partida (Tauler t) actu) (Moviment (Peca i b pos) pi pf c e m) = pIni && pFi && captura && escac && mat && potFerMov
 where
  pIni = checkPosicio pi
  pFi = checkPosicio pf
  captura = (not hiHaPecaFi && not c) || (hiHaPecaFi && c && diffColor) --mira si hi ha peca de diferent color a pFi, si ha de capturar
  escac = e == hiHaEscac (Tauler t) actu-- ha de mirar si esta fent escac si diu que en fa
  mat = True -- ha de mirar si esta fent mat si diu que en fa
  pecaAFi = buscarPeca (Tauler t) pf
  pecaAIni = buscarPeca (Tauler t) pi
  hiHaPecaFi = isJust pecaAFi
  diffColor = hiHaPecaFi && blanca (fromJust pecaAFi) /= b
  potFerMov =True -- not null (filter ((\x y -> x == y)pFi) (moviments (Peca i b pos) captura)) && not (alguEntre (Tauler t) pi pf)


--comprova rangs
checkPosicio :: Posicio -> Bool
checkPosicio (Posicio c i) = (i>=1 && i<9 && _c>=1 && _c< 9)
 where _c = charToInt c

data Direccio = U | D | L | R deriving (Eq, Show)

seguentPosicio :: Posicio -> Direccio -> Posicio
seguentPosicio p U = Posicio (succ (columna p)) (fila p)
seguentPosicio p D = Posicio (pred (columna p)) (fila p)
seguentPosicio p L = Posicio (columna p) (pred (fila p))
seguentPosicio p R = Posicio (columna p) (succ (fila p))

moure :: Posicio -> [Direccio] -> Posicio -- Donada una posicio final i una llista de direccions, retorna la posicio resultant
moure p d = foldl seguentPosicio p d

--pe2e4
--pd3xf3

pam :: [a -> b] -> a -> [b]
pam f x = map g f
    where g h = h x

columnPositions :: Posicio -> [Posicio] --Retorna totes les posicions que comparteixin columna amb la posició entrada
columnPositions p = map (Posicio (columna p))  [1..8]

rowPositions :: Posicio -> [Posicio] --Retorna totes les posicions que comparteixin fila amb la posició entrada
rowPositions p = pam (map Posicio ['a'..'h']) (fila p)

diagonalPositions :: Posicio -> [Posicio] --Retorna totes les posicions que comparteixin diagonal amb la posició entrada
diagonalPositions p = [Posicio c f | c <- ['a'..'h'],
                                   f <- [1..8],
                                   ((charToInt c-f)==(charToInt(columna p) - fila p)) || ((charToInt c-(8-f))==(charToInt(columna p) - (8-fila p)))  ]


--donada una Peca i sabent si captura o no, torna una llista de posicions on podria anar amb un tauler buit
moviments :: Peca -> Bool -> [Posicio]
moviments (Peca i b pos) c = case i of
 'P'  -> if b then if c then [(moure pos [U,L])] ++ [(moure pos [U,R])] else if ((fila pos == 2)) then [(moure pos [U])] ++ [(moure pos [U,U])] else [(moure pos [U])] else if c then [(moure pos [D,L])] ++ [(moure pos [D,R])] else if ((fila pos == 2)) then [(moure pos [D])] ++ [(moure pos [D,D])] else [(moure pos [D])]
 'C'  -> [(moure pos [U,U,L])  ]++[(moure pos [U,U,R]) ]++[(moure pos [R,R,U])]++[(moure pos [R,R,D]) ]++[(moure pos [D,D,R]) ]++[(moure pos [D,D,L]) ]++[(moure pos [L,L,D]) ]++[(moure pos [L,L,U])]
 'A'  -> diagonalPositions pos
 'D'  -> moviments (Peca 'T' b pos) False ++ moviments (Peca 'R' b pos) False ++ moviments (Peca 'A' b pos) False
 'R'  -> [(moure pos [U,L])] ++ [(moure pos [U])] ++[(moure pos [U,R])] ++[(moure pos [R])] ++ [(moure pos [D,R])] ++ [(moure pos [D])] ++ [(moure pos [D,L])] ++ [(moure pos [L])]
 'T'  -> (columnPositions pos) ++ (rowPositions pos)


-- donat un Tauler i dues posicions, ens diu si hi ha algú entre les dues posicions
   {-fi: filaInicial, ff: filaFinal, ci: columnaInicial, cf: columnaFinal-}
alguEntre :: Tauler -> Posicio -> Posicio -> Bool
alguEntre (Tauler t) (Posicio ci fi) (Posicio cf ff) = 
 if ci==cf then 
   if null (filterExtraCol (sameColumn) t ci fi ff) then False else True
 else if fi==ff then
   if null (filterExtraFil (sameRow) t fi ci cf) then False else True
 else if (fi- (charToInt ci))==(ff- (charToInt cf)) then
   if null (filterExtraDia (sameDiagonalR) t fi ff (charToInt ci) (charToInt cf)) then False else True
 else if (fi+ (charToInt ci))==(ff+ (charToInt cf)) then
   if null (filterExtraDia (sameDiagonalS) t fi ff (charToInt ci) (charToInt cf)) then False else True
 else False


-- retorna les peces que hi ha entre fi i ff a la columna col - per cada peca del tauler mira si compleix el filtre
filterExtraCol :: (Peca -> Char -> Int -> Int -> Bool) -> [Peca] -> Char -> Int -> Int -> [Peca]
filterExtraCol f [] col fi ff = []
filterExtraCol f [x] col fi ff  = if (f x col fi ff) then [x] else []
filterExtraCol f (x:xs) col fi ff  = if (f x col fi ff) then ([x] ++ filterExtraCol f xs col fi ff) else ([] ++ filterExtraCol f xs col fi ff)

-- retorna les peces que hi ha entre ci i cf a la fila fil - per cada peca del tauler mira si compleix el filtre
filterExtraFil :: (Peca -> Int -> Char -> Char -> Bool) -> [Peca] -> Int -> Char -> Char -> [Peca]
filterExtraFil f [] fil ci cf = []
filterExtraFil f [x] fil ci cf  = if (f x fil ci cf) then [x] else []
filterExtraFil f (x:xs) fil ci cf  = if (f x fil ci cf) then ([x] ++ filterExtraFil f xs fil ci cf) else ([] ++ filterExtraFil f xs fil ci cf)

-- retorna les peces que hi ha entre dues posicions en diagonal - per cada peca del tauler mira si compleix el filtre
filterExtraDia :: (Peca -> Int -> Int -> Int -> Int -> Bool) -> [Peca] -> Int -> Int -> Int -> Int -> [Peca]
filterExtraDia f [] fi ff ci cf = []
filterExtraDia f [x] fi ff ci cf  = if (f x fi ff ci cf) then [x] else []
filterExtraDia f (x:xs) fi ff ci cf  = if (f x fi ff ci cf) then ([x] ++ filterExtraDia f xs fi ff ci cf) else ([] ++ filterExtraDia f xs fi ff ci cf)

--retorna cert si la peca es troba a la columna (col) i entre les dues files (fi, ff)
sameColumn :: Peca -> Char -> Int -> Int -> Bool
sameColumn (Peca i b (Posicio c f)) col fi ff = ((col==c) && (if fi>ff then (f<fi && f>ff) else (f>fi && f<ff)))

--retorna cert si la peca es troba a la fila fil i entre dues columnes (ci, cf)
sameRow :: Peca -> Int -> Char -> Char -> Bool
sameRow (Peca i b (Posicio c f)) fil ci cf = ((fil==f) && (if ci>cf then (c<ci && c>cf) else (c>ci && c<cf)))

--retorna cert si la peca es troba a la diagonal de esq a dreta i entre dues columnes (ci, cf)
sameDiagonalR :: Peca -> Int -> Int -> Int -> Int -> Bool
sameDiagonalR (Peca i b (Posicio c f)) fi ff ci cf = if (f-(charToInt c)) == (fi-ci) then 
  (if ci<cf then ((charToInt c)>ci && (charToInt c)<cf) else ((charToInt c)<ci && (charToInt c)>cf)) else False

--retorna cert si la peca es troba a la diagonal de dreta a esq i entre dues columnes (ci, cf)
sameDiagonalS :: Peca -> Int -> Int -> Int -> Int -> Bool
sameDiagonalS (Peca i b (Posicio c f)) fi ff ci cf = if (f+(charToInt c)) == (fi+ci) then 
  if ci<cf then ((charToInt c)>ci && (charToInt c)<cf) else ((charToInt c)<ci && (charToInt c)>cf) else False

movTest1 = Moviment {peca = pe2b, posIni= e2,posFi = Posicio {columna = 'e', fila = 4},captura=True, escac=False, mat=False}
movTest2 = Moviment {peca = pe2b, posIni= e2,posFi = Posicio {columna = 'f', fila = 7},captura=True, escac=False, mat=False}
movTest3 = Moviment {peca = pe2b, posIni= e2,posFi = Posicio {columna = 'f', fila = 2},captura=True, escac=False, mat=False}

{-Funcions a fer:
* moviment: donada una Peca i posicio, torna una llista de posicions on podria anar amb un tauler buit
* alguEntre: donat un Tauler i dues posicions, ens diu si hi ha algú entre les dues posicions
* fesJugada: donat un Tauler i una Jugada, torna el nou Tauler amb la jugada aplicada
* escac: donat un bàndol, ens diu si el rei d'aquell bàndol està amenaçat
* jugadaLegal: certifica que la Jugada entrada és legal
* escacMat: certifica que un bàndol ha rebut escac i mat.
-}

hiHaEscac :: Tauler -> Bool -> Bool --HiHaEscac STUB
hiHaEscac t b = False