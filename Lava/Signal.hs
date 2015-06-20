{-# LANGUAGE StandaloneDeriving,DeriveFunctor,DeriveFoldable,
             DeriveTraversable,MultiParamTypeClasses,
             FlexibleInstances,ScopedTypeVariables #-}
module Lava.Signal (Signal(..),Symbol(..),S(..),symbol,unsymbol,
                    bool,inv,and2,or2,xor2,varBool,delayBool,
                    int,neg,divide,modulo,plus,times,gteInt,
                    ifInt,
                    varInt,delayInt,
                    high,low,ifBool,equalBool,equalInt,
                    eval,
                    getType,Type(..)
                    )
where

import Prelude hiding (sequence,any,sum,product,all,and,or,foldl)
import Data.Traversable
import Data.Foldable
import Lava.Ref
import Lava.Error
----------------------------------------------------------------
-- Signal, Symbol, S

newtype Signal a
  = Signal {unSignal :: Symbol}

newtype Symbol
  = Symbol (Ref (S Symbol))

data S s
  = Bool      Bool
  | Inv       s
  | And       s s
  | Or        s s
  | Xor       s s
  | VarBool   String
  | DelayBool s s

  | Int      Int
  | Neg      s
  | Div      s s
  | Mod      s s
  | Plus     s s
  | Times    s s
  | Gte      s s
  | Equal    s s
  | If       s s s
  | VarInt   String
  | DelayInt s s

deriving instance Functor S
deriving instance Foldable S
deriving instance Traversable S

symbol :: S Symbol -> Symbol
symbol = Symbol . ref

unsymbol :: Symbol -> S Symbol
unsymbol (Symbol r) = deref r

deriving instance Eq Symbol
deriving instance Eq (Signal a)

data Type = TBool | TInt

----------------------------------------------------------------
-- operations

-- on bits

bool :: Bool -> Signal Bool
bool b = lift0 (Bool b)

inv :: Signal Bool -> Signal Bool
inv = lift1 Inv

and2 :: (Signal Bool,Signal Bool) -> Signal Bool
and2  (x , y) = lift2 And x y

or2 :: (Signal Bool,Signal Bool) -> Signal Bool
or2  (x , y) = lift2 Or x y

xor2 :: (Signal Bool,Signal Bool) -> Signal Bool
xor2 (x , y) = lift2 Xor x y

varBool :: String -> Signal Bool
varBool s = lift0 (VarBool s)

delayBool :: Signal Bool -> Signal Bool -> Signal Bool
delayBool = lift2 DelayBool

int :: Int -> Signal Int
int n = lift0 (Int n)

neg :: Signal Int -> Signal Int
neg = lift1 Neg

divide :: Signal Int -> Signal Int -> Signal Int
divide = lift2 Div

modulo :: Signal Int -> Signal Int -> Signal Int
modulo = lift2 Mod

plus :: (Signal Int,Signal Int) -> Signal Int
plus (x , y) = lift2 Plus x y

times :: (Signal Int , Signal Int) -> Signal Int
times (x , y) = lift2 Times x y

gteInt :: Signal Int -> Signal Int -> Signal Bool
gteInt = lift2 Gte

equal2 :: (Signal Int , Signal Int) -> Signal Bool
equal2 (x , y) = lift2 Equal x y

ifInt :: Signal Bool -> (Signal Int, Signal Int) -> Signal a
ifInt c (x,y) = lift3 If c x y

varInt :: String -> Signal Int
varInt s = lift0 (VarInt s)

delayInt :: Signal Int -> Signal Int -> Signal Int
delayInt = lift2 DelayInt

----------------------------------------------------------------
-- liftings

lift0 :: S Symbol -> Signal a
lift0 oper = Signal (symbol oper)

lift1 :: (Symbol -> S Symbol) -> Signal a -> Signal b
lift1 oper (Signal a) = Signal (symbol (oper a))

lift2 :: (Symbol -> Symbol -> S Symbol) -> Signal a -> Signal b -> Signal c
lift2 oper (Signal a) (Signal b) = Signal (symbol (oper a b))

lift3 :: (Symbol -> Symbol -> Symbol -> S Symbol)
      -> Signal a -> Signal b -> Signal c -> Signal d
lift3 oper (Signal a) (Signal b) (Signal c) = Signal (symbol (oper a b c))

----------------------------------------------------------------
-- smart constructors

low :: Signal Bool
low  = bool False

high :: Signal Bool
high = bool True

ifBool :: Signal Bool -> (Signal Bool, Signal Bool) -> Signal Bool
ifBool c (x,y) = or2(and2(c,x),and2(inv c,y))

equalBool :: Signal Bool -> Signal Bool -> Signal Bool
equalBool x y = inv (xor2 (x,y))

equalInt :: Signal Int -> Signal Int -> Signal Bool
equalInt x y = equal2 (x,y)


----------------------------------------------------------------
-- evaluate

class Lift a b where
  lift :: S b ->  a

instance Lift Bool b where
  lift (Bool b) = b
  lift _        = undefined

instance Lift Int b where
  lift (Int i) = i
  lift _       = undefined

class CoLift a b where
  coLift :: a -> S b

instance CoLift Bool b where
  coLift = Bool

instance CoLift Int b where
  coLift = Int

liftF1 :: (Lift a c , CoLift b c) => (a -> b) -> S c -> S c
liftF1 f = coLift . f . lift

liftF2 :: (Lift a d , Lift b d , CoLift c d) => (a -> b -> c) -> S d -> S d -> S d
liftF2 f x y = coLift (f (lift x) (lift y))

liftF3 :: (Lift a e , Lift b e , Lift c e , CoLift d e) => (a -> b -> c -> d) -> S e -> S e -> S e -> S e
liftF3 f x y z = coLift (f (lift x) (lift y) (lift z))

eval :: S (S a) -> S a
eval s =
  case s of
    Bool b       -> coLift b
    Inv  b       -> liftF1 not b
    And  x y     -> liftF2 (&&) x y
    Or   x y     -> liftF2 (||) x y
    Xor  x y     -> liftF2 (\ m n -> if m then not n else n) x y

    Int n        -> coLift n
    Neg n        -> liftF1 (negate :: Int -> Int) n
    Div n1 n2    -> liftF2 (div  :: Int -> Int -> Int) n1 n2
    Mod n1 n2    -> liftF2 (mod  :: Int -> Int -> Int) n1 n2
    Plus x y     -> liftF2 ((+)  :: Int -> Int -> Int) x y
    Times x y    -> liftF2 ((*)  :: Int -> Int -> Int) x y
    Gte n1 n2    -> liftF2 ((>=) :: Int -> Int -> Bool) n1 n2
    Equal x y    -> liftF2 ((==) :: Int -> Int -> Bool) x y
    If l m n     -> liftF3 ((\ x y z -> if x then y else z) :: Bool -> Int -> Int -> Int) l m n

    DelayBool _ _ -> wrong Lava.Error.DelayEval
    DelayInt  _ _ -> wrong Lava.Error.DelayEval
    VarBool   _   -> wrong Lava.Error.VarEval
    VarInt    _   -> wrong Lava.Error.VarEval

instance Show (Signal a) where
  showsPrec n (Signal s) =
    showsPrec n s

instance Show Symbol where
  showsPrec n sym =
    showsPrec n (unsymbol sym)

deriving instance Show a => Show (S a )

----------------------------------------------------------------
-- the end.

getType :: S a -> Type
getType s =
  case s of
    Bool _        -> TBool
    Inv _         -> TBool
    And _ _       -> TBool
    Or _ _        -> TBool
    Xor _ _       -> TBool
    Int _         -> TInt
    Neg _         -> TInt
    Div _ _       -> TInt
    Mod _ _       -> TInt
    Plus _ _      -> TInt
    Times _ _     -> TInt
    Gte _ _       -> TBool
    Equal _ _     -> TBool
    If _ _ _      -> TInt
    DelayBool _ _ -> TBool
    DelayInt  _ _ -> TInt
    VarBool _     -> TBool
    VarInt  _     -> TInt
