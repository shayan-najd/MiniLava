module Lava.Combinational where

import Lava.Generic
import Lava.Signal
import Lava.Netlist
import Data.Traversable
import Lava.Error

import Lava.MyST
  ( newSTRef
  , readSTRef
  , writeSTRef
  , runST
  )

----------------------------------------------------------------
-- simulate

simulate :: Generic b => (a -> b) -> a -> b
simulate circ inp = runST (
  do sr <- netlistST new define (struct (circ inp))
     sa <- traverse (fmap symbol . readSTRef) sr
     let res = construct sa
     return res
  )
 where
  new =
    newSTRef (wrong Lava.Error.CombinationalLoop)

  define r s =
    do s' <- traverse readSTRef s
       writeSTRef r (eval s')

----------------------------------------------------------------
-- the end.
