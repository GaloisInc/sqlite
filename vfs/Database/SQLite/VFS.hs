{-# LANGUAGE ForeignFunctionInterface #-}
module Database.SQLite.VFS where

import Foreign
import Foreign.C
import Database.SQLite.Types
import Database.SQLite
import Data.Bits

openConnection' :: FilePath
                -> Maybe OpenFlags
                -> Maybe String
                -> IO SQLiteHandle
openConnection' file mb_mode mb_vfs =
  alloca $ \ptr ->
  withCString file $ \fp ->
  with_vfs $ \vfs ->
  do let mode = case mb_mode of
                  Nothing -> sQLITE_OPEN_READWRITE .|. sQLITE_OPEN_CREATE
                  Just m  -> m
     st <- sqlite3_open_v2 fp ptr mode vfs
     case st of
       0 -> newSQLiteHandle =<< peek ptr
       _ -> fail ("openDatabase: failed to open " ++ show st)


  where with_vfs f = case mb_vfs of
                       Nothing -> f nullPtr
                       Just x  -> withCString x f


foreign import ccall "little.h register_little_vfs"
  register_little_vfs :: Bool -> IO Status

foreign import ccall "little.h register_little_ro_vfs"
  register_little_ro_vfs :: Bool -> IO Status

foreign import ccall "sqlite3.h sqlite3_open_v2"
  sqlite3_open_v2 :: CString -> Ptr SQLite -> OpenFlags -> CString -> IO Status

