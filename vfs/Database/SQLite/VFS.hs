{-# LANGUAGE ForeignFunctionInterface #-}
module Database.SQLite.VFS where

import Database.SQLite.Types

foreign import ccall "little.h register_little_vfs"
  register_little_vfs :: Bool -> IO Status

foreign import ccall "little.h register_little_ro_vfs"
  register_little_ro_vfs :: Bool -> IO Status

