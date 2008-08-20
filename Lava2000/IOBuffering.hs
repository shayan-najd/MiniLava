module Lava2000.IOBuffering where

import IO
  ( hSetBuffering
  , stdout
  , BufferMode(..)
  )

noBuffering :: IO ()
noBuffering = hSetBuffering stdout NoBuffering
