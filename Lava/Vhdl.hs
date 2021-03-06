module Lava.Vhdl
  (compile
  )
 where

import Data.Foldable (toList)
import Lava.Signal
import Lava.Netlist
import Lava.Generic
import Lava.Error

import Data.List
  ( intersperse
  )

----------------------------------------------------------------
-- write vhdl

compile :: (Constructive a, Generic b) => (a -> b) -> String
compile circ = let inp = var "inp"
                   out = toList (struct (circ inp))
               in  writeDefinitions "program"
                   [v | VarBool v <- map unsymbol (toList (struct inp))]
                   out
----------------------------------------------------------------
-- definitions

writeDefinitions :: String -> [String] -> [Symbol] ->
                    String
writeDefinitions name inp out =
  let (g , o) = netgraph out
      outvs   = fmap w o
      out'    = ["outp_" ++ show i | i <- [0..(length out - 1)]]
  in  (unlines $
       [ "-- Generated by Lava 2000"
       , ""
       , "use work.all;"
       , ""
       , "entity"
       , "  " ++ name
       , "is"
       , "port"
       , "  -- clock"
       , "  ( clk : in bit"
       , ""
       , "  -- inputs"
       ]
       ++
       [ "  ; " ++ v ++ " : in bit" | v <- inp]
       ++
       [ ""
       , "  -- outputs"
       ]
       ++
       [ "  ; " ++ v ++ " : out bit" | v <- out']
       ++
       [ "  );"
       , "end entity " ++ name ++ ";"
       , ""
       , "architecture"
       , "  structural"
       , "of"
       , "  " ++ name
       , "is"
       ]
       ++
       [ "  signal " ++ w x ++ " : bit;" | (x , _) <- g]
       ++
       [ "begin"]
       ++
       [ define (w x) (fmap w s) | (x , s) <- g]
       ++
       [ ""
       , "  -- naming outputs"]
       ++
       [ define v' (VarBool v)
       | (v,v') <- outvs `zip` out']
       ++
       [ "end structural;"])
 where
  w x ="w" ++ show x

define :: String -> S String -> String
define v s =
           case s of
             Bool True     -> port "vdd"   []
             Bool False    -> port "gnd"   []
             Inv x         -> port "inv"   [x]
             And x y       -> port "and2"  [x,y]
             Or  x y       -> port "or2"   [x,y]
             Xor x y       -> port "xor2"  [x,y]
             VarBool s'    -> port "id"    [s']
             DelayBool x y -> port "delay" [x, y]
             _             -> wrong Lava.Error.NoArithmetic
           where
            port name' args =
                      "  "
                   ++ make 9 ("c_" ++ v)
                   ++ " : entity "
                   ++ make 5 name'
                   ++ " port map ("
                   ++ concat (intersperse ", " ("clk" : args ++ [v]))
                   ++ ");"
            make n ss = take (n `max` length ss) (ss ++ repeat ' ')



----------------------------------------------------------------
-- the end.
