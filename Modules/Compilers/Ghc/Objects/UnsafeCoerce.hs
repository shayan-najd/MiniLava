module UnsafeCoerce
  ( unsafeCoerce
  )
 where

import IOExts
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  , unsafePerformIO
  )
  
unsafeCoerce :: a -> b
unsafeCoerce a = unsafePerformIO $
  do writeIORef ref a
     readIORef ref
 where
  ref = unsafePerformIO $
    do newIORef undefined
