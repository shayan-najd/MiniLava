module Lava.Library.Arithmetic where

import Prelude hiding (abs,sum)
import Lava
import Lava.Library.Patterns
import Lava.Library.Operators

----------------------------------------------------------------
-- Basic Components

halfAdd :: Signal Bool -> Signal Bool -> (Signal Bool, Signal Bool)
halfAdd a b = (sum, carry)
  where
    sum   = xor2 a b
    carry = and2 a b

fullAdd :: Signal Bool -> Signal Bool -> Signal Bool ->
           (Signal Bool, Signal Bool)
fullAdd carryIn a b = (sum, carryOut)
  where
    (sum1, carry1) = halfAdd a b
    (sum, carry2)  = halfAdd carryIn sum1
    carryOut       = xor2 carry1 carry2

bitAdder :: Signal Bool -> [Signal Bool] ->
            ([Signal Bool], Signal Bool)
bitAdder = row halfAdd

adder :: Signal Bool -> [Signal Bool] -> [Signal Bool] ->
         ([Signal Bool], Signal Bool)
adder carryIn []     []     = ([], carryIn)
adder carryIn as     []     = bitAdder carryIn as
adder carryIn []     bs     = bitAdder carryIn bs
adder carryIn (a:as) (b:bs) = (s:ss , carryOut)
  where
    (s, carry)     = fullAdd carryIn a b
    (ss, carryOut) = adder carry as bs

binAdder :: [Signal Bool] -> [Signal Bool] -> [Signal Bool]
binAdder as bs = sum ++ [carryOut]
  where
    (sum, carryOut) = adder low as bs

bitMulti :: Signal Bool -> [Signal Bool] -> [Signal Bool]
bitMulti a bs = [and2 a b | b <- bs ]

multi :: [Signal Bool] -> [Signal Bool] -> [Signal Bool]
multi []     [] = []
multi as     [] = replicate (length as) low
multi []     bs = replicate (length bs) low
multi (a:as) bs = m : ms
  where
    (m:abs) = bitMulti a bs
    asbs    = multi as bs
    (ms,_)  = adder low abs asbs

numBreak :: Signal Int -> (Signal Bool, Signal Int)
numBreak num = (bit, num')
  where
    digit = imod num 2
    bit   = int2bit digit
    num'  = idiv num 2

int2bin :: Int -> Signal Int -> [Signal Bool]
int2bin 0 _   = []
int2bin n num = (bit:bits)
  where
    (bit,num') = numBreak num
    bits       = int2bin (n-1) num'

bin2int :: [Signal Bool] -> Signal Int
bin2int []     = 0
bin2int (b:bs) = num
  where
    num' = bin2int bs
    num  = bit2int b + 2 * num'

----------------------------------------------------------------
-- the end.
