module Lava2000.LavaDir where

import System
  ( getEnv
  )

import IO
  ( try
  )

getLavaDir :: IO FilePath
getLavaDir =
  do ees <- try (getEnv "LAVADIR")
     return $ case ees of
                Left _  -> {-INSERT LAVADIR-} "/home/emax/Program/Lava2000"
                Right s -> s
