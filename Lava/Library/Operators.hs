{-# LANGUAGE RankNTypes #-}
module Lava.Library.Operators where

import Lava.Signal
import Lava.Generic
import Lava.Error

infix  4 <==>
infixr 3 <&>
infixr 2 <|>, ==>, <==
infixr 2 <=>, <#>
infixr 1 |->

----------------------------------------------------------------------
-- Gates


nand2 :: Signal Bool -> Signal Bool -> Signal Bool
nand2 x y = inv (and2 x y)

nor2 :: Signal Bool -> Signal Bool -> Signal Bool
nor2 x y = inv (or2 x y)

xnor2 :: Signal Bool -> Signal Bool -> Signal Bool
xnor2 x y = inv (xor2 x y)

equiv :: Signal Bool -> Signal Bool -> Signal Bool
equiv x y = xnor2 x y

impl :: Signal Bool -> Signal Bool -> Signal Bool
impl x y = or2 (inv x) y

sub :: Signal Int -> Signal Int -> Signal Int
sub   x y = plus x (- y)

imod :: Signal Int -> Signal Int -> Signal Int
imod  x y = modulo x y

idiv :: Signal Int -> Signal Int -> Signal Int
idiv  x y = divide x y

----------------------------------------------------------------------
-- Binary Operators

(|->) :: forall a. Generic a => a -> a -> a
x |->  y = delay  x  y

(<==>) :: forall a. Generic a => a -> a -> Signal Bool
x <==> y = equal x y

(<&>) :: Signal Bool -> Signal Bool -> Signal Bool
x <&> y = and2  x y

(<|>) :: Signal Bool -> Signal Bool -> Signal Bool
x <|> y = or2   x y

(<#>) :: Signal Bool -> Signal Bool -> Signal Bool
x <#> y = xor2  x y

(<=>) :: Signal Bool -> Signal Bool -> Signal Bool
x <=> y = equiv x y

(==>) :: Signal Bool -> Signal Bool -> Signal Bool
x ==> y = impl  x y

(<==) :: Signal Bool -> Signal Bool -> Signal Bool
x <== y = impl  y x

(%%) :: Signal Int -> Signal Int -> Signal Int
x %% y     = imod x y

gte :: Signal Int -> Signal Int -> Signal Bool
gte x y = gteInt x y

(>>==) :: Signal Int -> Signal Int -> Signal Bool
x >>== y   = gte x y

imin :: Signal Int -> Signal Int -> Signal Int
imin x y = ifThenElse (x >>== y) y x

imax :: Signal Int -> Signal Int -> Signal Int
imax x y = ifThenElse (x >>== y) x y

class SignalInt a where
  toSignalInt   :: Signal a   -> Signal Int
  fromSignalInt :: Signal Int -> Signal a

instance SignalInt Int where
  toSignalInt   = id
  fromSignalInt = id

instance SignalInt a => Num (Signal a) where
  x + y    = fromSignalInt $ plus (toSignalInt x) (toSignalInt y)
  x - y    = fromSignalInt $ sub (toSignalInt x) (toSignalInt y)
  x * y    = fromSignalInt $ times (toSignalInt x) (toSignalInt y)
  negate x = fromSignalInt $ neg (toSignalInt x)

  fromInteger = fromSignalInt . int . fromInteger
  abs = undefined
  signum = undefined

instance SignalInt a => Fractional (Signal a) where
  x / y = fromSignalInt $ idiv (toSignalInt x) (toSignalInt y)
  fromRational = undefined

instance SignalInt a => Enum (Signal a) where
  toEnum n = fromSignalInt (int n)
  fromEnum (Signal s) =
    case unsymbol s of
      Int n -> n
      _     -> wrong Lava.Error.EnumOnSymbols

instance SignalInt a => Ord (Signal a) where
  min x y = fromSignalInt $ imin (toSignalInt x) (toSignalInt y)
  max x y = fromSignalInt $ imax (toSignalInt x) (toSignalInt y)
  compare = undefined


----------------------------------------------------------------------
-- Convert

int2bit :: Signal Int -> Signal Bool
int2bit n = n <==> (1 :: Signal Int)

bit2int :: Signal Bool -> Signal Int
bit2int b = ifThenElse b (1 :: Signal Int) 0

----------------------------------------------------------------------
-- the end.
