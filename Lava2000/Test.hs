module Lava2000.Test where

import Lava2000.Signal
import Lava2000.Sequential
import Lava2000.Generic

import Lava2000.LavaRandom
  ( newRnd
  )

----------------------------------------------------------------
-- test

test :: (Constructive a, Show b, Generic b) => (a -> b) -> IO [b]
test circ =
  do rnd <- newRnd
     let res = simulateSeq (\_ -> circ (random rnd)) (replicate 100 ())
     print res
     return res

----------------------------------------------------------------
-- the end.

