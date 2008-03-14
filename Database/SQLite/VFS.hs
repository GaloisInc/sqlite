{-# LANGUAGE ForeignFunctionInterface #-}
module Database.SQLite.VFS where

import Foreign.C

foreign import ccall "little.h register_little_vfs"
  register_little_vfs :: CInt -> IO CInt

foreign import ccall "little.h register_little_ro_vfs"
  register_little_ro_vfs :: CInt -> IO CInt

