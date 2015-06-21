module Lava.Library.SequentialCircuits where

import Prelude hiding (last,init)
import Lava
import Lava.Library.Operators

----------------------------------------------------------------
-- Sequential Circuits

edge :: Signal Bool -> Signal Bool
edge inp = change
  where
    inp'   = delay low inp
    change = xor2 inp inp'

toggle :: Signal Bool -> Signal Bool
toggle change = out
  where
    out' = delay low out
    out  = xor2 change out'

delayClk :: (Generic a) =>
            a -> Signal Bool -> a -> a
delayClk init clk inp = out
  where
    out = delay init val
    val = mux clk out inp

delayN :: (Generic b, Num a, Eq a) =>
          a -> b -> b -> b
delayN 0 _    inp = inp
delayN n init inp = out
  where
    out  = delay init rest
    rest = delayN (n-1) init inp

always :: Signal Bool -> Signal Bool
always inp = ok
  where
    sofar = delay high ok
    ok    = and2 inp sofar

constant :: Constructive a => a -> Signal Bool
constant x = ok
  where
    init = delay high low
    same = x <==> delay zero x
    ok   = always (init <|> same)

puls :: (Num a, Eq a) => a -> () -> Signal Bool
puls n () = out
  where
    out  = delayN (n-1) low last
    last = delay high out

outputList :: Generic a => [a] -> () -> a
outputList sigs () = out
  where
    out = foldr (|->) out sigs

rowSeq ::  Constructive a =>
           (a  -> b -> (c , a)) -> b -> c
rowSeq circ inp = out
  where
    carryIn         = delay zero carryOut
    (out, carryOut) = circ carryIn inp

rowSeqReset :: Constructive a =>
               (a -> b -> (c, a)) -> Signal Bool -> b -> c
rowSeqReset circ reset inp = out
  where
    carryIn         = delay zero carry
    carry           = mux reset carryOut zero
    (out, carryOut) = circ carryIn inp

rowSeqPeriod :: (Constructive b, Num a, Eq a) =>
                a -> (b -> c -> (d , b)) -> c -> d
rowSeqPeriod n circ inp = out
  where
    reset = puls n ()
    out   = rowSeqReset circ reset inp

----------------------------------------------------------------
-- the end.
