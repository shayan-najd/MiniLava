module Lava2000.Vhdl
  ( writeVhdl
  , writeVhdlInput
  , writeVhdlInputOutput
  )
 where

import Lava2000.Signal
import Lava2000.Netlist
import Lava2000.Generic
import Lava2000.Sequent
import Lava2000.Error
import Lava2000.LavaDir

import List
  ( intersperse
  , nub
  )

import IO
  ( openFile
  , IOMode(..)
  , hPutStr
  , hClose
  )

import System.IO
  ( stdout
  , BufferMode (..)
  , hSetBuffering
  )

import Data.IORef

import System.Cmd (system)
import System.Exit (ExitCode(..))

----------------------------------------------------------------
-- write vhdl

writeVhdl :: (Constructive a, Generic b) => String -> (a -> b) -> IO ()
writeVhdl name circ =
  do writeVhdlInput name circ (var "inp")

writeVhdlInput :: (Generic a, Generic b) => String -> (a -> b) -> a -> IO ()
writeVhdlInput name circ inp =
  do writeVhdlInputOutput name circ inp (symbolize "outp" (circ inp))

writeVhdlInputOutput :: (Generic a, Generic b)
                     => String -> (a -> b) -> a -> b -> IO ()
writeVhdlInputOutput name circ inp out =
  do writeItAll name inp (circ inp) out

writeItAll :: (Generic a, Generic b) => String -> a -> b -> b -> IO ()
writeItAll name inp out out' =
  do hSetBuffering stdout NoBuffering
     putStr ("Writing to file \"" ++ file ++ "\" ... ")
     writeDefinitions file name inp out out'
     putStrLn "Done."
 where
  file = name ++ ".vhd"

----------------------------------------------------------------
-- definitions

writeDefinitions :: (Generic a, Generic b)
                 => FilePath -> String -> a -> b -> b -> IO ()
writeDefinitions file name inp out out' =
  do firstHandle  <- openFile firstFile WriteMode
     secondHandle <- openFile secondFile WriteMode
     var <- newIORef 0

     hPutStr firstHandle $ unlines $
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
       ]

     hPutStr secondHandle $ unlines $
       [ "begin"
       ]

     let new =
           do n <- readIORef var
              let n' = n+1; v = "w" ++ show n'
              writeIORef var n'
              hPutStr firstHandle ("  signal " ++ v ++ " : bit;\n")
              return v

         define v s =
           case s of
             Bool True     -> port "vdd"  []
             Bool False    -> port "gnd"  []
             Inv x         -> port "inv"  [x]

             And []        -> define v (Bool True)
             And [x]       -> port "id"   [x]
             And [x,y]     -> port "and2" [x,y]
             And (x:xs)    -> define (w 0) (And xs)
                           >> define v (And [x,w 0])

             Or  []        -> define v (Bool False)
             Or  [x]       -> port "id"   [x]
             Or  [x,y]     -> port "or2"  [x,y]
             Or  (x:xs)    -> define (w 0) (Or xs)
                           >> define v (Or [x,w 0])

             Xor  []       -> define v (Bool False)
             Xor  [x]      -> port "id"   [x]
             Xor  [x,y]    -> port "xor2" [x,y]
             Xor  (x:xs)   -> define (w 0) (Or xs)
                           >> define (w 1) (Inv (w 0))
                           >> define (w 2) (And [x, w 1])

                           >> define (w 3) (Inv x)
                           >> define (w 4) (Xor xs)
                           >> define (w 5) (And [w 3, w 4])
                           >> define v     (Or [w 2, w 5])

             VarBool s     -> port "id" [s]
             DelayBool x y -> port "delay" [x, y]

             _             -> wrong Lava2000.Error.NoArithmetic
           where
            w i = v ++ "_" ++ show i

            port name args =
              do hPutStr secondHandle $
                      "  "
                   ++ make 9 ("c_" ++ v)
                   ++ " : entity "
                   ++ make 5 name
                   ++ " port map ("
                   ++ concat (intersperse ", " ("clk" : args ++ [v]))
                   ++ ");\n"

     outvs <- netlistIO new define (struct out)
     hPutStr secondHandle $ unlines $
       [ ""
       , "  -- naming outputs"
       ]

     sequence
       [ define v' (VarBool v)
       | (v,v') <- flatten outvs `zip` [ v' | VarBool v' <- outs' ]
       ]

     hPutStr secondHandle $ unlines $
       [ "end structural;"
       ]

     hClose firstHandle
     hClose secondHandle

     system ("cat " ++ firstFile ++ " " ++ secondFile ++ " > " ++ file)
     system ("rm " ++ firstFile ++ " " ++ secondFile)
     return ()
 where
  sigs x = map unsymbol . flatten . struct $ x

  inps  = sigs inp
  outs' = sigs out'

  firstFile  = file ++ "-1"
  secondFile = file ++ "-2"

  make n s = take (n `max` length s) (s ++ repeat ' ')


----------------------------------------------------------------
-- the end.
