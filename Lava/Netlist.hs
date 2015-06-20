module Lava.Netlist
  ( netgraph
  , netgraphF
  )
 where

import Lava.Generic
import Control.Applicative
import Data.Traversable
import Lava.Ref
import Lava.Signal
import Lava.MyST

----------------------------------------------------------------
-- netlist

netlistST :: Traversable f => ST s v -> (v -> S v -> ST s ()) ->
             f Symbol -> ST s (f v)
netlistST = netListM tableST findST extendST

netListM :: (Traversable f, Applicative m, Monad m) =>
            m a
            -> (a -> Ref (S Symbol) -> m (Maybe b))
            -> (a -> Ref (S Symbol) -> b -> m ())
            -> m b
            -> (b -> S b -> m ())
            -> f Symbol
            -> m (f b)
netListM tableM findM extendM new define symbols =
  do tab <- tableM
     traverse (gather tab findM extendM  new define) symbols

gather :: (Applicative m, Monad m) =>
          table
          -> (table -> Ref (S Symbol) -> m (Maybe b))
          -> (table -> Ref (S Symbol) -> b -> m ())
          -> m b
          -> (b -> S b -> m ())
          -> Symbol
          -> m b
gather tab findM extendM new define (Symbol sym) =
           do visited <- findM tab sym
              case visited of
                Just v  -> do return v
                Nothing -> do v <- new
                              extendM tab sym v
                              s <- traverse
                                   (gather tab findM extendM new define)
                                   (deref sym)
                              define v s
                              return v

netgraphF :: (Constructive a,Generic b) =>
            (a -> b) -> ([(Integer,S Integer)] , Struct Integer)
netgraphF f = netgraph (f (var "inp"))

netgraph :: Generic a => a -> ([(Integer, S Integer)], Struct Integer)
netgraph s = runST $
             do counter <- newSTRef 0
                nodes   <- newSTRef []
                rs      <- netlistST
                           (do n <- readSTRef counter
                               let n' = n + (1 :: Integer)
                               writeSTRef counter n'
                               return n')
                           (\ v ss -> do l <- readSTRef nodes
                                         writeSTRef nodes (l ++ [(v,ss)]))
                           (struct $ s)
                ns <- readSTRef nodes
                return (ns , rs)



----------------------------------------------------------------
-- the end.
