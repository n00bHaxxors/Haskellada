module Bloc where
  
import Posicio
import Moviment
import Data.List


data Bloc = Bloc { dimx :: Int,
    dimy :: Int,
    dimz :: Int,
    pos :: Posicio
    } deriving (Eq, Show)

--  xxxxxxxxx
--  xxPOOxxxx
--  xxOOOxxxx
--  xxxxxxxxx
--  xxxxxxxxx



-- ha de fer efectiu un moviment (si es legal) i portar-nos a l'estat que en resulti.
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

-- ens ha de donar les posicions que ocupa el bloc.
posBloc :: Bloc -> [Posicio]
posBloc b =
  do
    let posicions = [(x,y) | x <- [0 .. (dimx b - 1)], y <- [0 .. (dimy b - 1)]]
    map (sumar (pos b)) posicions