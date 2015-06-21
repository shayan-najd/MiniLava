{-# LANGUAGE StandaloneDeriving,DeriveFoldable,DeriveTraversable,DeriveFunctor,FlexibleInstances #-}
module Lava.Generic
  (Struct(..),Constructive(..),Generic(..),equal,delay,zeroize,symbolize,ifThenElse,mux) where

import Prelude hiding (concatMap)
import Data.Traversable
import Data.Foldable

import Lava.Signal
import Lava.Error

----------------------------------------------------------------
-- Struct

data Struct a
  = Compound [Struct a]
  | Object a
 deriving (Eq, Show)

deriving instance Functor Struct
deriving instance Foldable Struct
deriving instance Traversable Struct

----------------------------------------------------------------
-- Generic datatypes

class Generic a where
  struct    :: a -> Struct Symbol
  construct :: Struct Symbol -> a

instance Generic (Struct Symbol) where
  struct    = id
  construct = id

instance Generic Symbol where
  struct    s          = Object s
  construct (Object s) = s
  construct _          = undefined

instance Generic (Signal a) where
  struct    (Signal s) = Object s
  construct (Object s) = Signal s
  construct _          = undefined

instance Generic () where
  struct    ()            = Compound []
  construct (Compound []) = ()
  construct _             = undefined

instance Generic a => Generic [a] where
  struct    xs            = Compound (map struct xs)
  construct (Compound xs) = map construct xs
  construct _             = undefined

instance (Generic a, Generic b) => Generic (a,b) where
  struct    (a,b)            = Compound [struct a, struct b]
  construct (Compound [a,b]) = (construct a, construct b)
  construct _                = undefined

instance (Generic a, Generic b, Generic c) => Generic (a,b,c) where
  struct    (a,b,c)            = Compound [struct a, struct b, struct c]
  construct (Compound [a,b,c]) = (construct a, construct b, construct c)
  construct _                  = undefined

instance (Generic a, Generic b, Generic c, Generic d) => Generic (a,b,c,d) where
  struct    (a,b,c,d)            = Compound [struct a, struct b, struct c, struct d]
  construct (Compound [a,b,c,d]) = (construct a, construct b, construct c, construct d)
  construct _                    = undefined

instance (Generic a, Generic b, Generic c, Generic d, Generic e) => Generic (a,b,c,d,e) where
  struct    (a,b,c,d,e)            = Compound [struct a, struct b, struct c, struct d, struct e]
  construct (Compound [a,b,c,d,e]) = (construct a, construct b, construct c, construct d, construct e)
  construct _                      = undefined

instance (Generic a, Generic b, Generic c, Generic d, Generic e, Generic f) => Generic (a,b,c,d,e,f) where
  struct    (a,b,c,d,e,f)            = Compound [struct a, struct b, struct c, struct d, struct e, struct f]
  construct (Compound [a,b,c,d,e,f]) = (construct a, construct b, construct c, construct d, construct e, construct f)
  construct _                        = undefined

instance (Generic a, Generic b, Generic c, Generic d, Generic e, Generic f, Generic g) => Generic (a,b,c,d,e,f,g) where
  struct    (a,b,c,d,e,f,g)            = Compound [struct a, struct b, struct c, struct d, struct e, struct f, struct g]
  construct (Compound [a,b,c,d,e,f,g]) = (construct a, construct b, construct c, construct d, construct e, construct f, construct g)
  construct _                          = undefined

----------------------------------------------------------------
-- generic definitions

equal :: Generic a => a -> a -> Signal Bool
equal x y = eq (struct x) (struct y)
 where
  eq (Object a)    (Object b)    = case getTyp $ unsymbol a of
    TBool -> equalBool (Signal a) (Signal b)
    TInt  -> equalInt  (Signal a) (Signal b)
  eq (Compound as) (Compound bs) = eqs as bs
  eq _             _             = low

  eqs []     []     = high
  eqs (a:as) (b:bs) = and2 (eq a b) (eqs as bs)
  eqs _      _      = low

delay :: Generic a => a -> a -> a
delay xx yy = construct (del (struct xx) (struct yy))
 where
  del (Object a)    ~(Object b)    = Object $
                                     case getTyp $ unsymbol a of
    TBool -> unSignal $ delayBool (Signal a) (Signal b)
    TInt  -> unSignal $ delayInt  (Signal a) (Signal b)
  del (Compound as) ~(Compound bs) = Compound (lazyZipWith del as bs)
    where
      lazyZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
      lazyZipWith _ []     _  = []
      lazyZipWith f (x:xs) ys = f x (safe head ys) : lazyZipWith f xs (safe tail ys)
        where
          safe _  []  = wrong Lava.Error.IncompatibleStructures
          safe f' xs' = f' xs'

zeroize :: Generic a => a -> a
zeroize x = construct (zro (struct x))
 where
  zro :: Struct Symbol -> Struct Symbol
  zro (Object a) = case getTyp $ unsymbol a of
    TBool -> Object (symbol (Bool False))
    TInt  -> Object (symbol (Int 0))
  zro (Compound as) = Compound [zro a | a <- as]

symbolize :: Generic a => String -> a -> a
symbolize s x = construct (sym s (struct x))
 where
  sym s' (Object a)    = Object $ case getTyp $ unsymbol a of
    TBool -> symbol (VarBool s')
    TInt  -> symbol (VarInt s')
  sym s' (Compound as) = Compound [ sym (s' ++ "_" ++ show i) a
                                  | (a,i) <- as `zip` [0:: Integer ..]
                                  ]

ifThenElse :: Generic a => Signal Bool -> a -> a -> a
ifThenElse c x y  = construct (iff (struct x) (struct y))
  where
     iff (Object a) (Object b) = Object $ case getTyp $ unsymbol a of
       TBool -> unSignal $ ifBool c (Signal a) (Signal b)
       TInt  -> unSignal $ ifInt  c (Signal a) (Signal b)
     iff (Compound as) (Compound bs) =
       Compound [iff a b
                | (a , b) <- zip as bs]
     iff _             _             = error "Bad Conditional"

----------------------------------------------------------------
-- Constructive

class Generic a => Constructive a where
  zero   :: a
  var    :: String -> a

instance  Constructive (Signal Bool) where
  zero   = low
  var    = varBool

instance  Constructive (Signal Int) where
  zero   = int 0
  var    = varInt

instance Constructive () where
  zero       = ()
  var _      = ()

instance (Constructive a, Constructive b)
      => Constructive (a, b) where
  zero       = (zero, zero)
  var s      = (var (s ++ "_1"), var (s ++ "_2"))

instance (Constructive a, Constructive b, Constructive c)
      => Constructive (a, b, c) where
  zero     = (zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"))

instance (Constructive a, Constructive b, Constructive c, Constructive d)
      => Constructive (a, b, c, d) where
  zero     = (zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"))

instance (Constructive a, Constructive b, Constructive c, Constructive d, Constructive e)
      => Constructive (a, b, c, d, e) where
  zero     = (zero, zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"), var (s ++ "_5"))

instance (Constructive a, Constructive b, Constructive c, Constructive d, Constructive e, Constructive f)
      => Constructive (a, b, c, d, e, f) where
  zero     = (zero, zero, zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"), var (s ++ "_5"), var (s ++ "_6"))

instance (Constructive a, Constructive b, Constructive c, Constructive d, Constructive e, Constructive f, Constructive g)
      => Constructive (a, b, c, d, e, f, g) where
  zero     = (zero, zero, zero, zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"), var (s ++ "_5"), var (s ++ "_6"), var (s ++ "_7"))

{-

instance Choice b => Choice (a -> b) where
  ifThenElse cond (f, g) =
    \a -> ifThenElse cond (f a, g a)
-}

mux :: Generic a => Signal Bool -> a -> a -> a
mux c a b = ifThenElse c b a


----------------------------------------------------------------
-- the end.
