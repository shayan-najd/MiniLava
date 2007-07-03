module Satnik
  ( satnik
  )
 where

import Signal
import Netlist
import Generic
import Sequent
import Property
import Error
import LavaDir
import Verification

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

import IOBuffering
  ( noBuffering
  )

import Data.IORef

import System.Cmd (system)
import System.Exit (ExitCode(..))

----------------------------------------------------------------
-- satnik

satnik :: Checkable a => a -> IO ProofResult
satnik a =
  do checkVerifyDir
     noBuffering
     (props,_) <- properties a
     proveFile defsFile (writeDefinitions defsFile props)
 where
  defsFile = verifyDir ++ "/circuit.cnf"

----------------------------------------------------------------
-- definitions

writeDefinitions :: FilePath -> [Signal Bool] -> IO ()
writeDefinitions file props =
  do firstHandle  <- openFile firstFile WriteMode
     secondHandle <- openFile secondFile WriteMode
     var <- newIORef 0
     cls <- newIORef 0

     hPutStr firstHandle $ unlines $
       [ "c Generated by Lava2000"
       , "c "
       ]

     let new =
           do n <- readIORef var
              let n' = n+1
              writeIORef var n'
              return n'

         clause xs =
           do n <- readIORef cls
              let n' = n+1 in n' `seq` writeIORef cls n'
              hPutStr secondHandle (unwords [ show x | x <- xs ] ++ " 0\n")

         define v s =
           case s of
             Bool True     -> clause [ v ]
             Bool False    -> clause [ -v ]
             Inv x         -> clause [ -x, -v ] >> clause [ x, v ]

             And []        -> define v (Bool True)
             And xs        -> conjunction v xs

             Or  []        -> define v (Bool False)
             Or  xs        -> conjunction (-v) (map negate xs)

             Xor  []       -> define v (Bool False)
             Xor  xs       -> exactly1 v xs

             VarBool s     -> hPutStr firstHandle ("c " ++ s ++ " : " ++ show v ++ "\n")

             DelayBool x y -> wrong Error.DelayEval
             _             -> wrong Error.NoArithmetic
           where

            conjunction v xs =
              do clause (v : map negate xs)
                 sequence_ [ clause [ -v, x ] | x <- xs ]

            exactly1 v xs =
              do clause (-v : xs)
                 sequence_ [ clause [ -v, -x, -y ] | (x,y) <- pairs xs ]
                 sequence_ [ clause ( v : -x : ys) | (x,ys) <- pick xs ]

            pairs []     = []
            pairs (x:xs) = [ (x,y) | y <- xs ] ++ pairs xs

            pick []      = []
            pick (x:xs)  = (x,xs) : [ (y,x:ys) | (y,ys) <- pick xs ]

     outvs <- netlistIO new define (struct props)
     clause (map negate (flatten outvs))

     nvar <- readIORef var
     ncls <- readIORef cls
     hPutStr firstHandle ("p cnf " ++ show nvar ++ " " ++ show ncls ++ "\n")

     hClose firstHandle
     hClose secondHandle

     system ("cat " ++ firstFile ++ " " ++ secondFile ++ " > " ++ file)
     system ("rm " ++ firstFile ++ " " ++ secondFile)
     return ()
 where
  firstFile  = file ++ "-1"
  secondFile = file ++ "-2"

----------------------------------------------------------------
-- primitive proving

proveFile :: FilePath -> IO () -> IO ProofResult
proveFile file before =
  do putStr "Satnik: "
     before
     putStr "... "
     lavadir <- getLavaDir
     x <- system ( lavadir
                ++ "/Scripts/satnik.wrapper "
                ++ file
                ++ " -showTime"
                 )
     let res = case x of
                 ExitSuccess   -> Valid
                 ExitFailure 1 -> Indeterminate
                 ExitFailure _ -> Falsifiable
     putStrLn (show res ++ ".")
     return res

----------------------------------------------------------------
-- the end.

