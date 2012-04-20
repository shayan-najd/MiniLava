module Lava.Eprover
  ( eprover
  )
 where

import Lava.Signal
import Lava.Netlist
import Lava.Generic
import Lava.Sequent
import Lava.Property
import Lava.Error
import Lava.LavaDir
import Lava.Verification

import Data.List
  ( intersperse
  , nub
  )

import System.IO
  ( openFile
  , IOMode(..)
  , hPutStrLn
  , hClose
  )

import System.IO.Error
  ( try
  )

import Lava.IOBuffering
  ( noBuffering
  )

import Data.IORef

import System.Cmd (system)
import System.Exit (ExitCode(..))

----------------------------------------------------------------
-- eprover

eprover :: Checkable a => a -> IO ProofResult
eprover a =
  do checkVerifyDir
     noBuffering
     (props,_) <- properties a
     proveFile defsFile (writeDefinitions defsFile props)
 where
  defsFile = verifyDir ++ "/circuit.tptp"

----------------------------------------------------------------
-- definitions

writeDefinitions :: FilePath -> [Signal Bool] -> IO ()
writeDefinitions file props =
  do han <- openFile file WriteMode
     hPutStrLn han "% Generated by Lava\n"
     hPutStrLn han "% definitions:\n"
     ref <- newIORef (2 :: Int)

     let clause kind xs =
           hPutStrLn han $ unlines $
             [ "input_clause(name," ++ kind ++ ",["
             , concat (intersperse ", " xs)
             , "])."
             ]

         new =
           do n <- readIORef ref
              let n' = n+1
              n' `seq` writeIORef ref n'
              return ("*" ++ show n')

         define v s =
           do definition v $
                case s of
                  Bool True  -> int 1
                  Bool False -> int 0
                  VarBool s  -> op2 "mod" s (int 2)
                  Inv x      -> op1 "inv" x

                  And xs     -> opl (int 1) "and" xs
                  Or  xs     -> opl (int 0) "or" xs
                  Xor xs     -> xor xs

                  Int n      -> int n
                  Neg x      -> op1 "neg" x
                  Div x y    -> op2 "div" x y
                  Mod x y    -> op2 "mod" x y
                  Plus xs    -> opl (int 0) "add" xs
                  Times xs   -> opl (int 1) "mul" xs
                  Gte x y    -> op2 "gte" x y
                  If x y z   -> op3 "if" x y z
                  VarInt s   -> s

                  Equal []     -> int 1
                  Equal (x:xs) -> opl (int 1) "and" [op2 "eq" x y | y <- xs]

                  DelayBool x y -> wrong Lava.Error.DelayEval
                  DelayInt  x y -> wrong Lava.Error.DelayEval
           where
            definition v s =
              clause "axiom" [ "++equal(" ++ v ++ ":" ++ s ++ "," ++ v ++ ")" ]

            op1 f x     = f ++ "(" ++ x ++ ")"
            op2 f x y   = f ++ "(" ++ x ++ "," ++ y ++ ")"
            op3 f x y z = f ++ "(" ++ x ++ "," ++ y ++ "," ++ z ++ ")"

            opl z f []     = z
            opl z f [x]    = x
            opl z f (x:xs) = op2 f x (opl z f xs)

            int n = opl "cZero" "add" (replicate n "cOne")

            xor []     = int 0
            xor [x]    = x
            xor [x,y]  = op2 "xor" x y
            xor (x:xs) = op2 "or" (op2 "and" x (op1 "inv" (opl (int 0) "or" xs)))
                                  (op2 "and" (op1 "inv" x) (xor xs))

     outvs <- netlistIO new define (struct props)
     hPutStrLn han "% conjecture:\n"
     clause "conjecture" [ "--truth(" ++ v ++ ")" | v <- flatten outvs ]
     hClose han

     try (do readFile theoryFile
             system ("cat " ++ theoryFile ++ " >> " ++ file))

     return ()
 where
  theoryFile = "theory.tptp"

----------------------------------------------------------------
-- primitive proving

proveFile :: FilePath -> IO () -> IO ProofResult
proveFile file before =
  do putStr "Eprover: "
     before
     putStr "... "
     lavadir <- getLavaDir
     x <- system ( lavadir
                ++ "/Scripts/eprover.wrapper "
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

