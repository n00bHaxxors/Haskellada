module Posicio (
 posX,
 posY,
 sumarX,
 sumarY
) where

-- (x,y) del tauler
data Posicio = P Int Int deriving (Eq, Ord, Show, Read)

posX :: Posicio -> Int
posX (P x _) = x

posY :: Posicio -> Int
posY (P _ y) = y
