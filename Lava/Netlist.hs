module Lava.Netlist
  ( netlistIO
  , netlistST
  )
 where

import Control.Applicative
import Data.Traversable
import Lava.Ref
import Lava.Signal

import Lava.MyST
  ( ST
  )

----------------------------------------------------------------
-- netlist

netlistIO :: Traversable f => IO v -> (v -> S v -> IO ()) ->
             f Symbol -> IO (f v)
netlistIO = netListM tableIO findIO extendIO

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
     traverse (gather new define tab findM extendM) symbols

gather :: (Applicative m, Monad m) =>
          m b
          -> (b -> S b -> m ())
          -> a
          -> (a -> Ref (S Symbol) -> m (Maybe b))
          -> (a -> Ref (S Symbol) -> b -> m ())
          -> Symbol
          -> m b
gather new define tab findM extendM (Symbol sym) =
           do visited <- findM tab sym
              case visited of
                Just v  -> do return v
                Nothing -> do v <- new
                              extendM tab sym v
                              s <- traverse
                                   (gather new define tab findM extendM)
                                   (deref sym)
                              define v s
                              return v

----------------------------------------------------------------
-- the end.
