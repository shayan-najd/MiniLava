module Lava.Library.Patterns where

import Prelude hiding (abs)
-- import Lava

infixr 5 ->-
infixr 4 -|-

{-
This Lava module defines some often used wiring circuits
and connection patterns.
-}

----------------------------------------------------------------
-- Wiring Circuits

swap :: (a , b) -> (b , a)
swap  (a,b) = (b,a)

swapl :: [a] -> [a]
swapl [a,b] = [b,a]
swapl _     = undefined

copy :: a -> (a, a)
copy a      = (a,a)

riffle :: [a] -> [a]
riffle   = halveList ->- zipp ->- unpair

unriffle :: [a] -> [a]
unriffle = pair ->- unzipp ->- append

zipp :: ([a], [b]) -> [(a, b)]
zipp ([],   [])   = []
zipp (a:as, b:bs) = (a,b) : zipp (as, bs)
zipp _            = undefined

unzipp :: [(a, b)] -> ([a], [b])
unzipp []          = ([],   [])
unzipp ((a,b):abs) = (a:as, b:bs)
  where
    (as, bs) = unzipp abs

pair :: [a] -> [(a, a)]
pair (x:y:xs) = (x,y) : pair xs
pair _        = []

unpair :: [(a, a)] -> [a]
unpair ((x,y):xys) = x : y : unpair xys
unpair []          = []

halveList :: [a] -> ([a], [a])
halveList inps = (left,right)
  where
    left  = take half inps
    right = drop half inps
    half  = length inps `div` 2

append :: ([a], [a]) -> [a]
append (a,b) = a ++ b

----------------------------------------------------------------
-- Connection Patterns

serial :: (a -> b) -> (b -> c) -> a -> c
serial circ1 circ2 = circ2 . circ1

(->-) :: (a -> b) -> (b -> c) -> a -> c
circ1 ->- circ2    = serial circ1 circ2

compose :: [a -> a] -> a -> a
compose []           = id
compose (circ:circs) = circ ->- compose circs

composeN :: Int -> (a -> a) -> a -> a
composeN n circ = compose (replicate n circ)

par :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
par circ1 circ2 (a, b) = (circ1 a, circ2 b)

(-|-) :: (a -> c) -> (b -> d) -> (a , b) -> (c , d)
circ1 -|- circ2        = par circ1 circ2

parl :: ([a] -> [b]) -> ([a] -> [b]) -> [a] -> [b]
parl circ1 circ2 = halveList ->- (circ1 -|- circ2) ->- append

two :: ([a] -> [b]) -> [a] -> [b]
two circ = parl circ circ

ilv :: ([a] -> [b]) -> [a] -> [b]
ilv circ = unriffle ->- two circ ->- riffle

iter :: Int -> (a -> a) -> a -> a
iter 0 _    circ = circ
iter n comb circ = comb (iter (n-1) comb circ)

twoN :: Int -> ([a] -> [a]) -> [a] -> [a]
twoN n circ = iter n two circ

ilvN :: Int -> ([a] -> [b]) -> [a] -> [b]
ilvN n circ = iter n ilv circ

bfly :: Int -> ([a] -> [a]) -> [a] -> [a]
bfly 0 _    = id
bfly n circ = ilv (bfly (n-1) circ) ->- twoN (n-1) circ

pmap :: ((a, a) -> (b, b)) -> [a] -> [b]
pmap circ = pair ->- map circ ->- unpair

tri :: (a -> a) -> [a] -> [a]
tri _    []         = []
tri circ (inp:inps) = inp : (map circ ->- tri circ) inps

mirror :: (a -> b -> (d, c)) -> b -> a -> (c, d)
mirror circ a b = (c, d)
  where
    (d, c) = circ b a

row :: (a -> b -> (c, a)) -> a -> [b] -> ([c], a)
row _    carryIn []     = ([], carryIn)
row circ carryIn (a:as) = (b:bs, carryOut)
  where
    (b, carry)     = circ carryIn a
    (bs, carryOut) = row circ carry as

column :: (c -> a -> (a, b)) -> [c] -> a -> (a, [b])
column circ = mirror (row (mirror circ))

grid :: (a -> b -> (b, a)) -> [a] -> [b] -> ([b], [a])
grid circ = row (column circ)

----------------------------------------------------------------
-- the end.
