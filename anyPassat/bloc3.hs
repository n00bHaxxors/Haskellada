data Nat = Cero | Suc Nat deriving Show

--3.1

--
instance Eq Nat where
  Cero == Cero = True
  Suc x == Suc y = (x == y)
  _ == _  = False


--3.2

instance Ord Nat where
  Cero <= _ = True
  Suc x <= Suc y = (x <= y)
  _ <= _ = False

--3.3

instance Num Nat where
  Cero + x = x
  Suc x + y = Suc y + x 
  x - y = Suc x - Suc y
  Cero * _ = Cero
  abs x = x
  signum x = -x
  fromInteger x = Cero

--3.4

instance Enum Nat where
 toEnum :: Enum a -> Int
 fromEnum :: Enum a -> Int

 --3.5
 
instance Real Nat