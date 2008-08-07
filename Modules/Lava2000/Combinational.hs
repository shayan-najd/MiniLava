module Lava2000.Combinational where

import Lava2000.Ref
import Lava2000.Generic
import Lava2000.Signal
import Lava2000.Netlist
import Lava2000.Sequent
import Lava2000.Error

import Lava2000.MyST
  ( ST
  , STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , runST
  )

----------------------------------------------------------------
-- simulate

simulate :: Generic b => (a -> b) -> a -> b
simulate circ inp = runST (
  do sr <- netlistST new define (struct (circ inp))
     sa <- mmap (fmap symbol . readSTRef) sr
     let res = construct sa
     return res
  )
 where
  new =
    newSTRef (wrong Lava2000.Error.CombinationalLoop)

  define r s =
    do s' <- mmap readSTRef s
       writeSTRef r (eval s')

----------------------------------------------------------------
-- the end.

