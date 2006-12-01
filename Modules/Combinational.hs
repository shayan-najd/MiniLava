module Combinational where

import Ref
import Generic
import Signal
import Netlist
import Sequent
import Error

import MyST
  ( ST
  , STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , runST
  , stToIO
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
    newSTRef (wrong Error.CombinationalLoop)

  define r s =
    do s' <- mmap readSTRef s
       writeSTRef r (eval s')

----------------------------------------------------------------
-- simulate IO

simulateIO :: Generic b => (a -> IO b) -> a -> IO b
simulateIO circ inp =
  do out <- circ inp
     stToIO (
        do sr <- netlistST new define (struct out)
           sa <- mmap (fmap symbol . readSTRef) sr
           let res = construct sa
           return res
        )
 where
  new =
    newSTRef (wrong Error.CombinationalLoop)

  define r s =
    do s' <- mmap readSTRef s
       writeSTRef r (eval s')

----------------------------------------------------------------
-- the end.

