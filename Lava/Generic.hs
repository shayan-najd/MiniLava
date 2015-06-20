{-# LANGUAGE StandaloneDeriving,DeriveFoldable,DeriveTraversable,DeriveFunctor,FlexibleInstances #-}
module Lava.Generic
  (Struct(..),Constructive(..),Generic(..),Choice(..),delay,symbolize,equal,mux) where

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
-- Ops

data Ops
  = Ops { equalSymbol :: Symbol -> Symbol -> Signal Bool
        , delaySymbol :: Symbol -> Symbol -> Symbol
        , ifSymbol    :: Signal Bool -> (Symbol, Symbol) -> Symbol
        , varSymbol   :: String -> Symbol
        }

opsBool :: Ops
opsBool =
  Ops { equalSymbol = \x y     -> equalBool (Signal x) (Signal y)
      , delaySymbol = \x y     -> unSignal $ delayBool (Signal x) (Signal y)
      , ifSymbol    = \c (x,y) -> unSignal $ ifBool c  (Signal x,  Signal y)
      , varSymbol   = \s       -> symbol (VarBool s)
      }

opsInt :: Ops
opsInt =
  Ops { equalSymbol = \x y     -> equalInt (Signal x) (Signal y)
      , delaySymbol = \x y     -> unSignal $ delayInt (Signal x) (Signal y)
      , ifSymbol    = \c (x,y) -> unSignal $ ifInt c  (Signal x,  Signal y)
      , varSymbol   = \s       -> symbol (VarInt s)
      }

unSignal :: Signal a -> Symbol
unSignal (Signal s) = s

ops :: Symbol -> Ops
ops s =
  case unsymbol s of
    Bool _        -> opsBool
    Inv _         -> opsBool
    And _ _       -> opsBool
    Or _ _        -> opsBool
    Xor _ _       -> opsBool
    Int _         -> opsInt
    Neg _         -> opsInt
    Div _ _       -> opsInt
    Mod _ _       -> opsInt
    Plus _ _      -> opsInt
    Times _ _     -> opsInt
    Gte _ _       -> opsBool
    Equal _ _     -> opsBool
    If _ _ _      -> opsInt
    DelayBool _ _ -> opsBool
    DelayInt  _ _ -> opsInt
    VarBool _     -> opsBool
    VarInt  _     -> opsInt

----------------------------------------------------------------
-- generic definitions

equal :: Generic a => (a, a) -> Signal Bool
equal (x, y) = eq (struct x) (struct y)
 where
  eq (Object a)    (Object b)    = equalSymbol (ops a) a b
  eq (Compound as) (Compound bs) = eqs as bs
  eq _             _             = low

  eqs []     []     = high
  eqs (a:as) (b:bs) = and2 (eq a b, eqs as bs)
  eqs _      _      = low

delay :: Struct Symbol -> Struct Symbol -> Struct Symbol
delay x y = del x y
 where
  del (Object a)    ~(Object b)    = Object (delaySymbol (ops a) a b)
  del (Compound as) ~(Compound bs) = Compound (lazyZipWith del as bs)

symbolize :: String -> Struct Symbol -> Struct Symbol
symbolize s x = sym s x
 where
  sym s' (Object a)    = Object (varSymbol (ops a) s')
  sym s' (Compound as) = Compound [ sym (s' ++ "_" ++ show i) a
                                  | (a,i) <- as `zip` [0:: Integer ..]
                                  ]

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

----------------------------------------------------------------
-- Choice

class Choice a where
  ifThenElse :: Signal Bool -> (a, a) -> a

instance Choice Symbol where
  ifThenElse cond (x, y) = ifSymbol (ops x) cond (x, y)

instance Choice (Signal a) where
  ifThenElse cond (Signal x, Signal y) =
    Signal (ifThenElse cond (x, y))

instance Choice () where
  ifThenElse _ (_, _) = ()

instance Choice a => Choice [a] where
  ifThenElse cond (xs, ys) =
    strongZipWith (curry (ifThenElse cond)) xs ys

instance (Choice a, Choice b) => Choice (a,b) where
  ifThenElse cond ((x1,x2),(y1,y2)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2))

instance (Choice a, Choice b, Choice c) => Choice (a,b,c) where
  ifThenElse cond ((x1,x2,x3),(y1,y2,y3)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3))

instance (Choice a, Choice b, Choice c, Choice d) => Choice (a,b,c,d) where
  ifThenElse cond ((x1,x2,x3,x4),(y1,y2,y3,y4)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4))

instance (Choice a, Choice b, Choice c, Choice d, Choice e) => Choice (a,b,c,d,e) where
  ifThenElse cond ((x1,x2,x3,x4,x5),(y1,y2,y3,y4,y5)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4), ifThenElse cond (x5,y5))

instance (Choice a, Choice b, Choice c, Choice d, Choice e, Choice f) => Choice (a,b,c,d,e,f) where
  ifThenElse cond ((x1,x2,x3,x4,x5,x6),(y1,y2,y3,y4,y5,y6)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4), ifThenElse cond (x5,y5),
     ifThenElse cond (x6,y6))

instance (Choice a, Choice b, Choice c, Choice d, Choice e, Choice f, Choice g) => Choice (a,b,c,d,e,f,g) where
  ifThenElse cond ((x1,x2,x3,x4,x5,x6,x7),(y1,y2,y3,y4,y5,y6,y7)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4), ifThenElse cond (x5,y5),
     ifThenElse cond (x6,y6), ifThenElse cond (x7,y7))

instance Choice b => Choice (a -> b) where
  ifThenElse cond (f, g) =
    \a -> ifThenElse cond (f a, g a)

mux :: Choice a => (Signal Bool, (a, a)) -> a
mux (cond, (a, b)) = ifThenElse cond (b, a)

----------------------------------------------------------------
-- helper functions

strongZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
strongZipWith f (x:xs) (y:ys) = f x y : strongZipWith f xs ys
strongZipWith _ []     []     = []
strongZipWith _ _      _      = wrong Lava.Error.IncompatibleStructures

lazyZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
lazyZipWith _ []     _  = []
lazyZipWith f (x:xs) ys = f x (safe head ys) : lazyZipWith f xs (safe tail ys)
 where
  safe _  []  = wrong Lava.Error.IncompatibleStructures
  safe f' xs' = f' xs'

----------------------------------------------------------------
-- the end.
