module Lava.Vhdl
  (vhdl
  )
 where

import Data.Foldable (toList)
import Lava.Signal
import Lava.Netlist
import Lava.Generic
import Lava.Error
import Control.Monad.Writer

import Data.List
  ( intersperse
  )

----------------------------------------------------------------
-- write vhdl

vhdl :: (Constructive a, Generic b) => String -> (a -> b) -> String
vhdl name circ = let inp = var "inp"
                     out = circ inp
                 in  writeDefinitions name inp out
                     (symbolize "outp" out)

----------------------------------------------------------------
-- definitions

writeDefinitions :: (Generic a, Generic b)
                 => String -> a -> b -> b -> String
writeDefinitions name inp out out' =
  let (g , o)  = netgraph out
      outvs = fmap w o
  in   (unlines $
       [ "-- Generated by Lava 2000"
       , ""
       , "use work.all;"
       , ""
       , "entity"
       , "  " ++ name
       , "is"
       , "port"
       , "  -- clock"
       , "  ( " ++ "clk" ++ " : in bit"
       , ""
       , "  -- inputs"
       ] ++
       [ "  ; " ++ v ++ " : in bit"
       | VarBool v <- inps
       ] ++
       [ ""
       , "  -- outputs"
       ] ++
       [ "  ; " ++ v ++ " : out bit"
       | VarBool v <- outs'
       ] ++
       [ "  );"
       , "end entity " ++ name ++ ";"
       , ""
       , "architecture"
       , "  structural"
       , "of"
       , "  " ++ name
       , "is"
       ]) ++
       (concat
       ["  signal " ++ w x ++ " : bit;\n"
       | (x , _) <- g]) ++
       "\n" ++
       (unlines
       [ "begin"
       ]) ++
       (concat
       [snd $ runWriter $ define (w x) (fmap w s)
       | (x , s) <- g]) ++
       (unlines
       [ ""
       , "  -- naming outputs"
       ]) ++
       (concat
       [snd $ runWriter $ define v' (VarBool v)
       | (v,v') <- toList outvs `zip` [ v' | VarBool v' <- outs' ]
       ]) ++
       unlines
       [ "end structural;"
       ]
 where
  w x ="w" ++ show x
  sigs x = map unsymbol . toList . struct $ x

  inps  = sigs inp
  outs' = sigs out'

define :: String -> S String -> Writer String ()
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
              do tell $
                      "  "
                   ++ make 9 ("c_" ++ v)
                   ++ " : entity "
                   ++ make 5 name'
                   ++ " port map ("
                   ++ concat (intersperse ", " ("clk" : args ++ [v]))
                   ++ ");\n"
            make n ss = take (n `max` length ss) (ss ++ repeat ' ')



----------------------------------------------------------------
-- the end.
