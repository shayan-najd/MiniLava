{-# LANGUAGE RankNTypes #-}
module Lava.Operators where

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

and2 :: (Signal Bool, Signal Bool) -> Signal Bool
and2 (x, y) = andl [x, y]

or2 :: (Signal Bool, Signal Bool) -> Signal Bool
or2  (x, y) = orl  [x, y]

xor2 :: (Signal Bool, Signal Bool) -> Signal Bool
xor2 (x, y) = xorl [x, y]

nand2 :: (Signal Bool, Signal Bool) -> Signal Bool
nand2 = inv . and2

nor2 :: (Signal Bool, Signal Bool) -> Signal Bool
nor2  = inv . or2

xnor2 :: (Signal Bool, Signal Bool) -> Signal Bool
xnor2 = inv . xor2

equiv :: (Signal Bool, Signal Bool) -> Signal Bool
equiv (x, y) = xnor2 (x, y)

impl :: (Signal Bool, Signal Bool) -> Signal Bool
impl  (x, y) = or2   (inv x, y)

nandl :: [Signal Bool] -> Signal Bool
nandl = inv . andl

norl :: [Signal Bool] -> Signal Bool
norl  = inv . orl

plus :: (Signal Int, Signal Int) -> Signal Int
plus  (x, y) = plusl [x,y]

sub :: (Signal Int, Signal Int) -> Signal Int
sub   (x, y) = plusl [x, neg y]

times :: (Signal Int, Signal Int) -> Signal Int
times (x, y) = timesl [x, y]

imod :: (Signal Int, Signal Int) -> Signal Int
imod  (x, y) = modulo x y

idiv :: (Signal Int, Signal Int) -> Signal Int
idiv  (x, y) = divide x y

----------------------------------------------------------------------
-- Binary Operators

(|->) :: forall a. Generic a => a -> a -> a
x |->  y = delay  x  y

(<==>) :: forall a. Generic a => a -> a -> Signal Bool
x <==> y = equal (x, y)

(<&>) :: Signal Bool -> Signal Bool -> Signal Bool
x <&> y = and2  (x, y)

(<|>) :: Signal Bool -> Signal Bool -> Signal Bool
x <|> y = or2   (x, y)

(<#>) :: Signal Bool -> Signal Bool -> Signal Bool
x <#> y = xor2  (x, y)

(<=>) :: Signal Bool -> Signal Bool -> Signal Bool
x <=> y = equiv (x, y)

(==>) :: Signal Bool -> Signal Bool -> Signal Bool
x ==> y = impl  (x, y)

(<==) :: Signal Bool -> Signal Bool -> Signal Bool
x <== y = impl  (y, x)

(%%) :: Signal Int -> Signal Int -> Signal Int
x %% y     = imod (x, y)

gte :: (Signal Int, Signal Int) -> Signal Bool
gte (x, y) = gteInt x y

(>>==) :: Signal Int -> Signal Int -> Signal Bool
x >>== y   = gte (x, y)

imin :: (Signal Int, Signal Int) -> Signal Int
imin (x, y) = ifThenElse (x >>== y) (y, x)

imax :: (Signal Int, Signal Int) -> Signal Int
imax (x, y) = ifThenElse (x >>== y) (x, y)

class SignalInt a where
  toSignalInt   :: Signal a   -> Signal Int
  fromSignalInt :: Signal Int -> Signal a

instance SignalInt Int where
  toSignalInt   = id
  fromSignalInt = id

instance SignalInt a => Num (Signal a) where
  x + y    = fromSignalInt $ plus (toSignalInt x, toSignalInt y)
  x - y    = fromSignalInt $ sub (toSignalInt x, toSignalInt y)
  x * y    = fromSignalInt $ times (toSignalInt x, toSignalInt y)
  negate x = fromSignalInt $ neg (toSignalInt x)

  fromInteger = fromSignalInt . int . fromInteger
  abs = undefined
  signum = undefined

instance SignalInt a => Fractional (Signal a) where
  x / y = fromSignalInt $ idiv (toSignalInt x, toSignalInt y)
  fromRational = undefined

instance SignalInt a => Enum (Signal a) where
  toEnum n = fromSignalInt (int n)
  fromEnum (Signal s) =
    case unsymbol s of
      Int n -> n
      _     -> wrong Lava.Error.EnumOnSymbols

instance SignalInt a => Ord (Signal a) where
  min x y = fromSignalInt $ imin (toSignalInt x, toSignalInt y)
  max x y = fromSignalInt $ imax (toSignalInt x, toSignalInt y)
  compare = undefined


----------------------------------------------------------------------
-- Convert

int2bit :: Signal Int -> Signal Bool
int2bit n = n <==> (1 :: Signal Int)

bit2int :: Signal Bool -> Signal Int
bit2int b = ifThenElse b (1 :: Signal Int, 0)

----------------------------------------------------------------------
-- the end.
