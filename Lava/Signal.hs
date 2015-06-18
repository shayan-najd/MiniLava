{-# LANGUAGE StandaloneDeriving,DeriveFunctor,DeriveFoldable,
             DeriveTraversable,MultiParamTypeClasses,
             FlexibleInstances,ScopedTypeVariables #-}
module Lava.Signal (Signal(..),Symbol(..),S(..),symbol,unsymbol,
                    bool,inv,andl,orl,xorl,varBool,delayBool,
                    int,neg,divide,modulo,plusl,timesl,gteInt,
                    equall,ifInt,
                    varInt,delayInt,
                    high,low,ifBool,equalBool,equalInt,
                    eval
                    )
where

import Prelude hiding (sequence,any,sum,product,all,and,or,foldl)
import Data.Traversable
import Data.Foldable
import Data.List(nub)
import Lava.Ref
import Lava.Error
----------------------------------------------------------------
-- Signal, Symbol, S

newtype Signal a
  = Signal Symbol

newtype Symbol
  = Symbol (Ref (S Symbol))

data S s
  = Bool      Bool
  | Inv       s
  | And       [s]
  | Or        [s]
  | Xor       [s]
  | VarBool   String
  | DelayBool s s

  | Int      Int
  | Neg      s
  | Div      s s
  | Mod      s s
  | Plus     [s]
  | Times    [s]
  | Gte      s s
  | Equal    [s]
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

----------------------------------------------------------------
-- operations

-- on bits

bool :: Bool -> Signal Bool
bool b = lift0 (Bool b)

inv :: Signal Bool -> Signal Bool
inv = lift1 Inv

andl :: [Signal Bool] -> Signal Bool
andl = liftl And

orl :: [Signal Bool] -> Signal Bool
orl  = liftl Or

xorl :: [Signal Bool] -> Signal Bool
xorl = liftl Xor

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

plusl :: [Signal Int] -> Signal Int
plusl  = liftl Plus

timesl :: [Signal Int] -> Signal Int
timesl = liftl Times

gteInt :: Signal Int -> Signal Int -> Signal Bool
gteInt = lift2 Gte

equall :: [Signal Int] -> Signal Bool
equall = liftl Equal

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

liftl :: ([Symbol] -> S Symbol) -> [Signal a] -> Signal c
liftl oper sigas = Signal (symbol (oper (map (\(Signal a) -> a) sigas)))

----------------------------------------------------------------
-- smart constructors

low :: Signal Bool
low  = bool False

high :: Signal Bool
high = bool True

ifBool :: Signal Bool -> (Signal Bool, Signal Bool) -> Signal Bool
ifBool c (x,y) = orl[andl[c,x],andl[inv c,y]]

equalBool :: Signal Bool -> Signal Bool -> Signal Bool
equalBool x y = inv (xorl [x,y])

equalInt :: Signal Int -> Signal Int -> Signal Bool
equalInt x y = equall [x,y]


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

liftFL :: (CoLift b c , Lift a c) => ([a] -> b) -> [S c] -> S c
liftFL f = coLift . f . fmap lift

eval :: S (S a) -> S a
eval s =
  case s of
    Bool b       -> coLift b
    Inv  b       -> liftF1 not b
    And xs       -> liftFL and xs
    Or xs        -> liftFL or  xs
    Xor xs       -> liftFL ((1 ==) . length . filter id) xs

    Int n        -> coLift n
    Neg n        -> liftF1 (negate :: Int -> Int) n
    Div n1 n2    -> liftF2 (div :: Int -> Int -> Int) n1 n2
    Mod n1 n2    -> liftF2 (mod :: Int -> Int -> Int) n1 n2
    Plus xs      -> liftFL (sum :: [Int] -> Int) xs
    Times xs     -> liftFL (product :: [Int] -> Int) xs
    Gte n1 n2    -> liftF2 ((>=) :: Int -> Int -> Bool) n1 n2
    Equal xs     -> liftFL (((<= 1) . length  . nub)  :: [Int] -> Bool) xs
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

instance Show a => Show (S a) where
  showsPrec n s =
    case s of
      Bool True  -> showString "high"
      Bool False -> showString "low"

      Inv x      -> showString "inv"  . showList [x]
      And xs     -> showString "andl" . showList xs
      Or  xs     -> showString "orl"  . showList xs
      Xor xs     -> showString "xorl" . showList xs

      Int   i    -> showsPrec n i
      Neg   x    -> showString "-" . showsPrec n x
      Div   x y  -> showString "idiv" . showList [x,y]
      Mod   x y  -> showString "imod" . showList [x,y]
      Plus  xs   -> showString "plusl" . showList xs
      Times xs   -> showString "timesl" . showList xs
      Gte   x y  -> showString "gte" . showList [x,y]
      Equal xs   -> showString "equall" . showList xs
      If x y z   -> showString "ifThenElse" . showList [x,y,z]

      DelayBool x y -> showString "delay" . showList [x,y]
      DelayInt  x y -> showString "delay" . showList [x,y]

      VarBool s'    -> showString s'
      VarInt  s'    -> showString s'


----------------------------------------------------------------
-- the end.
