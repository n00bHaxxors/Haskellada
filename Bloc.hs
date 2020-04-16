module Bloc where
  
import Posicio
import Moviment
import Data.List
import Data.Maybe


data Bloc = Bloc { dimx :: Int,
    dimy :: Int,
    dimz :: Int,
    pos :: Posicio
    } deriving (Eq, Show,Ord)

mostrarBloc :: Bloc -> IO()
mostrarBloc b = putStrLn "bloc"

-- efectua un moviment sobre un bloc.
moure :: Bloc -> Moviment -> Bloc
moure b U =
  do
    let newpos = sumar (pos b) (0,-(dimz b))
    Bloc {dimx = dimx b, dimy = dimz b, dimz = dimy b, pos = newpos}
moure b D =
  do
    let newpos = sumar (pos b) (0,dimy b)
    Bloc {dimx = dimx b, dimy = dimz b, dimz = dimy b, pos = newpos}
moure b R =
  do
    let newpos = sumar (pos b) (dimx b,0)
    Bloc {dimx = dimz b, dimy = dimy b, dimz = dimx b, pos = newpos}
moure b L =
  do
    let newpos = sumar (pos b) (-(dimz b),0)
    Bloc {dimx = dimz b, dimy = dimy b, dimz = dimx b, pos = newpos}
moure b N = b

-- ens dona les posicions que ocupa el bloc del tauler.
posBloc :: Bloc -> [Posicio]
posBloc b =
  do
    let posicions = [(x,y) | x <- [0 .. (dimx b - 1)], y <- [0 .. (dimy b - 1)]]
    map (sumar (pos b)) posicions

buscarTamany :: [String] -> Int
buscarTamany tauler = maximum (map (\x -> foldl(\x y -> x + if y =='S' then 1 else 0) 0 x) tauler)

--Funcions per crear dades a partir de l entrada
crearBloc :: Int -> [String] -> Bloc
crearBloc z tauler = result
  where
    result = Bloc x y z (Posicio posx posy)
    x = buscarTamany tauler
    y = buscarTamany (transpose tauler)
    posx = fromJust (elemIndex True (map (\x -> if (find (=='S') x) == Nothing then False else True) (transpose tauler)))
    posy = fromJust (elemIndex True (map (\x -> if (find (=='S') x) == Nothing then False else True) tauler))
    -- TODO

