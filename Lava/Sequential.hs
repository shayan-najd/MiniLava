module Lava.Sequential
  ( simulateSeq
  )
 where

import Lava.Signal
import Lava.Netlist
import Data.Foldable (toList)
import Data.Traversable
import Lava.Generic
import Control.Applicative

import Lava.MyST
  ( ST
  , STRef
  , newSTRef
  , readSTRef
  , writeSTRef
  , unsafeInterleaveST
  , runST
  )

----------------------------------------------------------------
-- wire datatype

type Var s
  = (STRef s (S Symbol), STRef s (Wire s))

data Wire s
  = Wire
    { dependencies :: [Var s]
    , kick         :: ST s ()
    }

----------------------------------------------------------------
-- simulate

simulateSeq :: (Generic a, Generic b) => (a -> b) -> [a] -> [b]
simulateSeq _    []   = []
simulateSeq circ inps = runST (
  do roots <- newSTRef []

     let new = (,) <$> newSTRef (error "val?")
                   <*> newSTRef (error "wire?")

         define r s =
           case s of
             DelayBool s' s'' -> delay' s' s''
             DelayInt  s' s'' -> delay' s' s''
             _ ->
               do relate r (toList s) $
                    eval `fmap` traverse (readSTRef . fst) s
          where
           delay' ri@(rinit,_) r1@(pre,_) =
               do state <- newSTRef Nothing
                  r2 <- new
                  rs <- readSTRef roots
                  writeSTRef roots (r2:rs)

                  relate r [ri] $
                    do ms <- readSTRef state
                       case ms of
                         Just s' -> return s'
                         Nothing ->
                           do s' <- readSTRef rinit
                              writeSTRef state (Just s')
                              return s'

                  relate r2 [r,r1] $
                    do s' <- readSTRef pre
                       writeSTRef state (Just s')
                       return s'

     sr   <- netlistST new define (struct (circ (input inps)))
     rs   <- readSTRef roots
     step <- drive (toList sr ++ rs)

     outs <- lazyloop $
       do step
          s <- traverse (fmap symbol . readSTRef . fst) sr
          return (construct s)

     return (takes inps outs)
  )

-- evaluation order

relate :: Var s -> [Var s] -> ST s (S Symbol) -> ST s ()
relate (rval, rwir) rs f =
  do writeSTRef rwir $
       Wire{ dependencies = rs
           , kick = do b <- f
                       writeSTRef rval b
           }

drive :: [Var s] -> ST s (ST s ())
drive [] =
  do return (return ())

drive ((_,rwir):rs) =
  do wire <- readSTRef rwir
     writeSTRef rwir (error "detected combinational loop")
     driv1 <- drive (dependencies wire)
     writeSTRef rwir $
       Wire { dependencies = [], kick = return () }
     driv2 <- drive rs
     return $
       do driv1
          kick wire
          driv2

----------------------------------------------------------------
-- helper functions

lazyloop :: ST s a -> ST s [a]
lazyloop m =
  do a  <- m
     as <- unsafeInterleaveST (lazyloop m)
     return (a:as)

input :: Generic a => [a] -> a
input xs = out
 where
  out = foldr delay out xs

takes :: [a] -> [b] -> [b]
takes []     _      = []
takes _      []     = error "not defined!"
takes (_:xs) (y:ys) = y : takes xs ys


----------------------------------------------------------------
-- the end.
