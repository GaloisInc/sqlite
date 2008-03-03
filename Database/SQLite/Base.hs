{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Module    : Database.SQLite.Base
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
-- Bindings to the SQLite C interface.
--
-- The documentation for these functions is at:
--
-- * <http://www.sqlite.org/c3ref/funclist.html>
--
module Database.SQLite.Base
       ( sqlite3_libversion
       , sqlite3_libversion_number
       , sqlite3_close
       , sqlite3_exec
       , sqlite3_extended_result_codes
       , sqlite3_last_insert_rowid
       , sqlite3_changes
       , sqlite3_total_changes
       , sqlite3_interrupt
       , sqlite3_complete
       , sqlite3_complete16
       , sqlite3_busy_handler
       , sqlite3_busy_timeout
       , sqlite3_get_table
       , sqlite3_free_table
       , sqlite3_malloc
       , sqlite3_realloc
       , sqlite3_free
{-
       , sqlite3_memory_used
       , sqlite3_memory_highwater
-}
       , sqlite3_set_authorizer
       , sqlite3_trace
       , sqlite3_profile
       , sqlite3_progress_handler
       , sqlite3_open
       , sqlite3_open16
--       , sqlite3_open_v2
       , sqlite3_errcode
       , sqlite3_errmsg

       , sqlite3_prepare

       , sqlite3_bind_blob
       , sqlite3_bind_double
       , sqlite3_bind_int
       , sqlite3_bind_int64
       , sqlite3_bind_null
       , sqlite3_bind_text
       , sqlite3_bind_value
       , sqlite3_bind_zeroblob
       , sqlite3_bind_parameter_count
       , sqlite3_bind_parameter_name
       , sqlite3_bind_parameter_index

       , sqlite3_clear_bindings
       , sqlite3_column_count
       , sqlite3_column_name
       , sqlite3_column_decltype
       , sqlite3_step
       , sqlite3_data_count

       , sqlite3_column_blob
       , sqlite3_column_bytes
       , sqlite3_column_bytes16
       , sqlite3_column_double
       , sqlite3_column_int
       , sqlite3_column_int64
       , sqlite3_column_text
       , sqlite3_column_text16
       , sqlite3_column_type
       , sqlite3_column_value

       , sqlite3_finalize
       , sqlite3_create_function

       , sqlite3_value_blob
       , sqlite3_value_bytes
       , sqlite3_value_bytes16
       , sqlite3_value_double
       , sqlite3_value_int
       , sqlite3_value_int64
       , sqlite3_value_text
       , sqlite3_value_text16
       , sqlite3_value_text16le
       , sqlite3_value_text16be
       , sqlite3_value_numeric_type
       , sqlite3_value_type
       , sqlite3_aggregate_context
       , sqlite3_user_data

       , sqlite3_get_auxdata
       , sqlite3_set_auxdata

       , sqlite3_static_destructor
       , sqlite3_transient_destructor

       , sqlite3_result_blob
       , sqlite3_result_double
       , sqlite3_result_error
       , sqlite3_result_error16
       , sqlite3_result_error_toobig
{-
       , sqlite3_result_error_nomem
-}
       , sqlite3_result_int
       , sqlite3_result_int64
       , sqlite3_result_null
       , sqlite3_result_text
       , sqlite3_result_text16
       , sqlite3_result_text16le
       , sqlite3_result_text16be
       , sqlite3_result_value
       , sqlite3_result_zeroblob

       , sqlite3_create_collation
       , sqlite3_create_collation16
       , sqlite3_create_collation_v2
       , sqlite3_collation_needed
       , sqlite3_collation_needed16

       , sqlite3_sleep
       , sqlite3_set_temp_directory
       , sqlite3_get_temp_directory
       , sqlite3_get_autocommit
       , sqlite3_db_handle
       , sqlite3_commit_hook
       , sqlite3_rollback_hook
       , sqlite3_update_hook
       , sqlite3_enable_shared_cache
{-
       , sqlite3_release_memory
       , sqlite3_soft_heap_limit
-}

       , sqlite3_blob_open
       , sqlite3_blob_close
       , sqlite3_blob_bytes
       , sqlite3_blob_read
       , sqlite3_blob_write

         -- helpful callback constructors:
       , ExecHandler
       , FreeHandler
       , UpdateHook
       , FilterHandler
       , StepHandler
       , FinalizeContextHandler
       , CompareHandler
       , CollationHandler
       , CollationHandler16
       , mkExecHandler
       , mkFreeHandler
       , mkUpdateHook
       , mkFilterHandler
       , mkStepHandler
       , mkFinalizeContextHandler
       , mkCompareHandler
       , mkCollationHandler
       , mkCollationHandler16
       ) where

import Database.SQLite.Types

import Foreign.C
import Foreign.Ptr

-- the various callback function types and constructors for their
-- Haskell wrappers:

type ExecHandler
  =  SQLiteCallbackUserData
  -> CInt
  -> Ptr CString
  -> Ptr CString
  -> IO Status

foreign import ccall "wrapper"
   mkExecHandler :: ExecHandler -> IO (SQLiteCallback ExecHandler)

type FreeHandler = SQLiteCallbackUserData -> IO ()

foreign import ccall "wrapper"
   mkFreeHandler :: FreeHandler -> IO (SQLiteCallback FreeHandler)

type UpdateHook
  = SQLiteCallbackUserData
  -> CInt
  -> CString
  -> CString
  -> SQLiteInt64 -> IO ()

foreign import ccall "wrapper"
   mkUpdateHook :: UpdateHook -> IO (SQLiteCallback UpdateHook)

type FilterHandler
 =  SQLiteCallbackUserData
 -> IO Status

foreign import ccall "wrapper"
   mkFilterHandler :: FilterHandler -> IO (SQLiteCallback FilterHandler)

type FinalizeContextHandler = SQLiteContext -> IO ()

foreign import ccall "wrapper"
   mkFinalizeContextHandler
     :: FinalizeContextHandler
     -> IO (SQLiteCallback FinalizeContextHandler)

type StepHandler
 =  SQLiteContext
 -> CInt
 -> Ptr SQLiteValue
 -> IO ()

foreign import ccall "wrapper"
   mkStepHandler :: StepHandler -> IO (SQLiteCallback StepHandler)

type CompareHandler
 =  SQLiteCallbackUserData
 -> CInt
 -> Ptr ()
 -> CInt
 -> Ptr ()
 -> IO CInt

foreign import ccall "wrapper"
   mkCompareHandler :: CompareHandler -> IO (SQLiteCallback CompareHandler)

type CollationHandler
 =  SQLiteCallbackUserData
 -> SQLite
 -> TextEncodeFlag
 -> CString
 -> IO ()

foreign import ccall "wrapper"
   mkCollationHandler :: CollationHandler -> IO (SQLiteCallback CollationHandler)

type CollationHandler16
 =  SQLiteCallbackUserData
 -> SQLite
 -> TextEncodeFlag
 -> SQLiteUTF16
 -> IO ()

foreign import ccall "wrapper"
   mkCollationHandler16 :: CollationHandler16 -> IO (SQLiteCallback CollationHandler16)

-- Binding the main API:

foreign import ccall "sqlite3.h sqlite3_libversion" 
  sqlite3_libversion :: IO CString

foreign import ccall "sqlite3.h sqlite3_libversion_number" 
  sqlite3_libversion_number :: IO CString

foreign import ccall "sqlite3.h sqlite3_close"
  sqlite3_close :: SQLite -> IO Status

foreign import ccall "sqlite3.h sqlite3_exec"
  sqlite3_exec :: SQLite
               -> CString
               -> SQLiteCallback ExecHandler
               -> SQLiteCallbackUserData
               -> Ptr CString
               -> IO Status

foreign import ccall "sqlite3.h sqlite3_extended_result_codes"
  sqlite3_extended_result_codes :: SQLite -> Bool -> IO Status

foreign import ccall "sqlite3.h sqlite3_last_insert_rowid"
  sqlite3_last_insert_rowid :: SQLite -> IO SQLiteInt64

foreign import ccall "sqlite3.h sqlite3_changes"
  sqlite3_changes :: SQLite -> IO CInt

foreign import ccall "sqlite3.h sqlite3_total_changes"
  sqlite3_total_changes :: SQLite -> IO CInt

foreign import ccall "sqlite3.h sqlite3_interrupt"
  sqlite3_interrupt :: SQLite -> IO ()

foreign import ccall "sqlite3.h sqlite3_complete"
  sqlite3_complete :: CString -> IO Bool

foreign import ccall "sqlite3.h sqlite3_complete16"
  sqlite3_complete16 :: SQLiteUTF16 -> IO Bool

foreign import ccall "sqlite3.h sqlite3_busy_handler"
  sqlite3_busy_handler :: SQLite -> FunPtr (Ptr () -> CInt -> IO CInt) -> Ptr () -> IO Status

foreign import ccall "sqlite3.h sqlite3_busy_timeout"
  sqlite3_busy_timeout :: SQLite -> CInt -> IO Status

foreign import ccall "sqlite3.h sqlite3_get_table"
  sqlite3_get_table :: SQLite
                    -> CString
                    -> Ptr (Ptr CString)
                    -> Ptr CInt
                    -> Ptr CInt
                    -> Ptr (Ptr CString)
                    -> IO Status

foreign import ccall "sqlite3.h sqlite3_free_table"
  sqlite3_free_table :: Ptr (Ptr CString) -> IO ()

foreign import ccall "sqlite3.h sqlite3_malloc"
  sqlite3_malloc :: CInt -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_realloc"
  sqlite3_realloc :: Ptr () -> CInt -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_free"
  sqlite3_free :: Ptr () -> IO ()

{-
foreign import ccall "sqlite3.h sqlite3_memory_used"
  sqlite3_memory_used :: IO SQLiteInt64

foreign import ccall "sqlite3.h sqlite3_memory_highwater"
  sqlite3_memory_highwater :: CInt -> IO SQLiteInt64
-}

foreign import ccall "sqlite3.h sqlite3_set_authorizer"
  sqlite3_set_authorizer :: SQLite -> FunPtr (Ptr () -> CInt -> CString -> CString -> CString -> CString -> IO Status) -> Ptr () -> IO Status

foreign import ccall "sqlite3.h sqlite3_trace"
  sqlite3_trace :: SQLite -> FunPtr (Ptr () -> CString -> IO ()) -> Ptr () -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_profile"
  sqlite3_profile :: SQLite -> FunPtr (Ptr () -> CString -> SQLiteInt64 -> IO ()) -> Ptr () -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_progress_handler"
  sqlite3_progress_handler :: SQLite -> CInt -> FunPtr (Ptr () -> IO CInt) -> Ptr () -> IO ()

foreign import ccall "sqlite3.h sqlite3_open"
  sqlite3_open :: CString -> Ptr SQLite -> IO Status

foreign import ccall "sqlite3.h sqlite3_open16"
  sqlite3_open16 :: SQLiteUTF16 -> Ptr SQLite -> IO Status

{-
foreign import ccall "sqlite3.h sqlite3_open_v2"
  sqlite3_open_v2 :: CString -> Ptr SQLite -> OpenFlags -> CString -> IO Status
-}

foreign import ccall "sqlite3.h sqlite3_errcode"
  sqlite3_errcode :: SQLite -> IO Status

foreign import ccall "sqlite3.h sqlite3_errmsg"
  sqlite3_errmsg :: SQLite -> IO CString

foreign import ccall "sqlite3.h sqlite3_prepare_v2"
  sqlite3_prepare :: SQLite -> CString -> CInt -> Ptr SQLiteStmt -> Ptr CString -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_blob"
  sqlite3_bind_blob :: SQLiteStmt -> CInt -> Ptr () -> CInt -> FunPtr (Ptr () -> IO ()) -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_double"
  sqlite3_bind_double :: SQLiteStmt -> CInt -> Double -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_int"
  sqlite3_bind_int :: SQLiteStmt -> CInt -> CInt -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_int64"
  sqlite3_bind_int64 :: SQLiteStmt -> CInt -> SQLiteInt64 -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_null"
  sqlite3_bind_null :: SQLiteStmt -> CInt -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_text"
  sqlite3_bind_text :: SQLiteStmt -> CInt -> CString -> CInt -> FunPtr (Ptr () -> IO ()) -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_value"
  sqlite3_bind_value :: SQLiteStmt -> CInt -> SQLiteValue -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_zeroblob"
  sqlite3_bind_zeroblob :: SQLiteStmt -> CInt -> CInt -> IO Status

foreign import ccall "sqlite3.h sqlite3_bind_parameter_count"
  sqlite3_bind_parameter_count :: SQLiteStmt -> IO CInt

foreign import ccall "sqlite3.h sqlite3_bind_parameter_name"
  sqlite3_bind_parameter_name :: SQLiteStmt -> CInt -> IO CString

foreign import ccall "sqlite3.h sqlite3_bind_parameter_index"
  sqlite3_bind_parameter_index :: SQLiteStmt -> CString -> IO CInt

foreign import ccall "sqlite3.h sqlite3_clear_bindings"
  sqlite3_clear_bindings :: SQLiteStmt -> IO Status

foreign import ccall "sqlite3.h sqlite3_column_count"
  sqlite3_column_count :: SQLiteStmt -> IO CInt

foreign import ccall "sqlite3.h sqlite3_column_name"
  sqlite3_column_name :: SQLiteStmt -> CInt -> IO CString

{- Not compiled into the Windows version of the lib by default, hence leave out
   for now:
foreign import ccall "sqlite3.h sqlite3_column_database_name"
  sqlite3_column_database_name :: SQLiteStmt -> CInt -> IO CString

foreign import ccall "sqlite3.h sqlite3_column_table_name"
  sqlite3_column_table_name :: SQLiteStmt -> CInt -> IO CString

foreign import ccall "sqlite3.h sqlite3_column_origin_name"
  sqlite3_column_origin_name :: SQLiteStmt -> CInt -> IO CString
-}
foreign import ccall "sqlite3.h sqlite3_column_decltype"
  sqlite3_column_decltype :: SQLiteStmt -> CInt -> IO CString

foreign import ccall "sqlite3.h sqlite3_step"
  sqlite3_step :: SQLiteStmt -> IO Status

foreign import ccall "sqlite3.h sqlite3_data_count"
  sqlite3_data_count :: SQLiteStmt -> IO Status

foreign import ccall "sqlite3.h sqlite3_column_blob"
  sqlite3_column_blob :: SQLiteStmt -> CInt -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_column_bytes"
  sqlite3_column_bytes :: SQLiteStmt -> CInt -> IO CInt

foreign import ccall "sqlite3.h sqlite3_column_bytes16"
  sqlite3_column_bytes16 :: SQLiteStmt -> CInt -> IO CInt

foreign import ccall "sqlite3.h sqlite3_column_double"
  sqlite3_column_double :: SQLiteStmt -> CInt -> IO Double

foreign import ccall "sqlite3.h sqlite3_column_int"
  sqlite3_column_int :: SQLiteStmt -> CInt -> IO CInt

foreign import ccall "sqlite3.h sqlite3_column_int64"
  sqlite3_column_int64 :: SQLiteStmt -> CInt -> IO SQLiteInt64

foreign import ccall "sqlite3.h sqlite3_column_text"
  sqlite3_column_text :: SQLiteStmt -> CInt -> IO CString

foreign import ccall "sqlite3.h sqlite3_column_text16"
  sqlite3_column_text16 :: SQLiteStmt -> CInt -> IO SQLiteUTF16

foreign import ccall "sqlite3.h sqlite3_column_type"
  sqlite3_column_type :: SQLiteStmt -> CInt -> IO CInt

foreign import ccall "sqlite3.h sqlite3_column_value"
  sqlite3_column_value :: SQLiteStmt -> CInt -> IO SQLiteValue

foreign import ccall "sqlite3.h sqlite3_finalize"
  sqlite3_finalize :: SQLiteStmt -> IO Status

foreign import ccall "sqlite3.h sqlite3_create_function"
  sqlite3_create_function :: SQLite
                          -> CString
                          -> CInt
                          -> TextEncodeFlag
                          -> SQLiteCallbackUserData
                          -> SQLiteCallback StepHandler
                          -> SQLiteCallback StepHandler
                          -> SQLiteCallback FinalizeContextHandler
                          -> IO CInt

foreign import ccall "sqlite3.h sqlite3_value_blob"
  sqlite3_value_blob :: SQLiteValue -> IO SQLiteBLOB

foreign import ccall "sqlite3.h sqlite3_value_bytes"
  sqlite3_value_bytes :: SQLiteValue -> IO CInt

foreign import ccall "sqlite3.h sqlite3_value_bytes16"
  sqlite3_value_bytes16 :: SQLiteValue -> IO CInt

foreign import ccall "sqlite3.h sqlite3_value_double"
  sqlite3_value_double :: SQLiteValue -> IO Double

foreign import ccall "sqlite3.h sqlite3_value_int"
  sqlite3_value_int :: SQLiteValue -> IO CInt

foreign import ccall "sqlite3.h sqlite3_value_int64"
  sqlite3_value_int64 :: SQLiteValue -> IO SQLiteInt64

foreign import ccall "sqlite3.h sqlite3_value_text"
  sqlite3_value_text :: SQLiteValue -> IO CString

foreign import ccall "sqlite3.h sqlite3_value_text16"
  sqlite3_value_text16 :: SQLiteValue -> IO SQLiteUTF16

foreign import ccall "sqlite3.h sqlite3_value_text16le"
  sqlite3_value_text16le :: SQLiteValue -> IO SQLiteUTF16

foreign import ccall "sqlite3.h sqlite3_value_text16be"
  sqlite3_value_text16be :: SQLiteValue -> IO SQLiteUTF16

foreign import ccall "sqlite3.h sqlite3_value_numeric_type"
  sqlite3_value_numeric_type :: SQLiteValue -> IO CInt

foreign import ccall "sqlite3.h sqlite3_value_type"
  sqlite3_value_type :: SQLiteValue -> IO FundamentalDatatype

foreign import ccall "sqlite3.h sqlite3_aggregate_context"
  sqlite3_aggregate_context :: SQLiteContext -> CInt -> IO SQLiteContextBuffer

foreign import ccall "sqlite3.h sqlite3_user_data"
  sqlite3_user_data :: SQLiteContext -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_get_auxdata"
  sqlite3_get_auxdata :: SQLiteContext -> CInt -> IO (Ptr ())

foreign import ccall "sqlite3.h sqlite3_set_auxdata"
  sqlite3_set_auxdata :: SQLiteContext -> CInt -> Ptr () -> SQLiteCallback FreeHandler -> IO ()

foreign import ccall "sqlite3.h get_SQLITE_STATIC"
  sqlite3_static_destructor :: SQLiteCallback FreeHandler

foreign import ccall "sqlite3.h get_SQLITE_TRANSIENT"
  sqlite3_transient_destructor :: SQLiteCallback FreeHandler

foreign import ccall "sqlite3.h sqlite3_result_blob"
  sqlite3_result_blob :: SQLiteContext -> Ptr () -> CInt -> SQLiteCallback FreeHandler -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_double"
  sqlite3_result_double :: SQLiteContext -> Double -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_error"
  sqlite3_result_error :: SQLiteContext -> CString -> CInt -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_error16"
  sqlite3_result_error16 :: SQLiteContext -> SQLiteUTF16 -> CInt -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_error_toobig"
  sqlite3_result_error_toobig :: SQLiteContext -> IO ()

{-
foreign import ccall "sqlite3.h sqlite3_result_error_nomem"
  sqlite3_result_error_nomem :: SQLiteContext -> IO ()
-}

foreign import ccall "sqlite3.h sqlite3_result_int"
  sqlite3_result_int :: SQLiteContext -> CInt -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_int64"
  sqlite3_result_int64 :: SQLiteContext -> SQLiteInt64 -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_null"
  sqlite3_result_null :: SQLiteContext -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_text"
  sqlite3_result_text :: SQLiteContext
                      -> CString
                      -> CInt
                      -> SQLiteCallback FreeHandler
                      -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_text16"
  sqlite3_result_text16 :: SQLiteContext -> SQLiteUTF16 -> CInt
                        -> SQLiteCallback FreeHandler
                        -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_text16le"
  sqlite3_result_text16le :: SQLiteContext -> SQLiteUTF16 -> CInt
                          -> SQLiteCallback FreeHandler -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_text16be"
  sqlite3_result_text16be :: SQLiteContext -> SQLiteUTF16 -> CInt
                          -> SQLiteCallback FreeHandler -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_value"
  sqlite3_result_value :: SQLiteContext -> SQLiteValue -> IO ()

foreign import ccall "sqlite3.h sqlite3_result_zeroblob"
  sqlite3_result_zeroblob :: SQLiteContext -> CInt -> IO ()

foreign import ccall "sqlite3.h sqlite3_create_collation"
  sqlite3_create_collation 
          :: SQLite -> CString -> TextEncodeFlag
          -> SQLiteCallbackUserData
          -> SQLiteCallback CompareHandler
          -> IO Status

foreign import ccall "sqlite3.h sqlite3_create_collation16"
  sqlite3_create_collation16
          :: SQLite -> SQLiteUTF16 -> TextEncodeFlag
          -> SQLiteCallbackUserData
          -> SQLiteCallback CompareHandler
          -> IO Status

foreign import ccall "sqlite3.h sqlite3_create_collation_v2"
  sqlite3_create_collation_v2 :: SQLite -> CString -> TextEncodeFlag
                              -> SQLiteCallbackUserData
                              -> SQLiteCallback CompareHandler
                              -> SQLiteCallback FreeHandler
                              -> IO Status

foreign import ccall "sqlite3.h sqlite3_collation_needed"
  sqlite3_collation_needed :: SQLite -> SQLiteCallbackUserData
                           -> SQLiteCallback CollationHandler
                           -> IO Status

foreign import ccall "sqlite3.h sqlite3_collation_needed16"
  sqlite3_collation_needed16 :: SQLite -> SQLiteCallbackUserData
                             -> SQLiteCallback CollationHandler16
                             -> IO Status

{-
foreign import ccall "sqlite3.h sqlite3_key"
  sqlite3_key :: SQLite -> Ptr () -> CInt -> IO Status

foreign import ccall "sqlite3.h sqlite3_rekey"
  sqlite3_rekey :: SQLite -> Ptr () -> CInt -> IO Status
-}
foreign import ccall "sqlite3.h sqlite3_sleep"
  sqlite3_sleep :: CInt -> IO Status

foreign import ccall "sqlite3-local.h sqlite3_set_temp_directory"
  sqlite3_set_temp_directory :: CString -> IO ()

foreign import ccall "sqlite3-local.h sqlite3_get_temp_directory"
  sqlite3_get_temp_directory :: IO CString

foreign import ccall "sqlite3.h sqlite3_get_autocommit"
  sqlite3_get_autocommit :: SQLite -> IO Bool

foreign import ccall "sqlite3.h sqlite3_db_handle"
  sqlite3_db_handle :: SQLiteStmt -> IO SQLite

foreign import ccall "sqlite3.h sqlite3_commit_hook"
  sqlite3_commit_hook :: SQLite
                      -> SQLiteCallback FilterHandler
                      -> SQLiteCallbackUserData
                      -> IO (SQLiteCallback FilterHandler)

foreign import ccall "sqlite3.h sqlite3_rollback_hook"
  sqlite3_rollback_hook :: SQLite
                        -> SQLiteCallback FreeHandler
                        -> SQLiteCallbackUserData
                        -> IO (SQLiteCallback FreeHandler)

foreign import ccall "sqlite3.h sqlite3_update_hook"
  sqlite3_update_hook :: SQLite
                      -> SQLiteCallback UpdateHook
                      -> SQLiteCallbackUserData
                      -> IO (SQLiteCallback FreeHandler)

foreign import ccall "sqlite3.h sqlite3_enable_shared_cache"
  sqlite3_enable_shared_cache :: CInt -> IO CInt

{-
foreign import ccall "sqlite3.h sqlite3_release_memory"
  sqlite3_release_memory :: CInt -> IO Status

foreign import ccall "sqlite3.h sqlite3_soft_heap_limit"
  sqlite3_soft_heap_limit :: CInt -> IO ()

foreign import ccall "sqlite3.h sqlite3_table_column_metadata"
  sqlite3_table_column_metadata 
          :: SQLite
          -> CString
          -> CString
          -> CString
          -> Ptr CString
          -> Ptr CString
          -> Ptr Bool
          -> Ptr Bool
          -> Ptr Bool
          -> IO Status

foreign import ccall "sqlite3.h sqlite3_load_extension"
  sqlite3_load_extension :: SQLite -> CString -> CString -> Ptr CString -> IO Status

foreign import ccall "sqlite3.h sqlite3_enable_load_extension"
  sqlite3_enable_load_extension :: SQLite -> CInt -> IO CInt

foreign import ccall "sqlite3.h sqlite3_auto_extension"
  sqlite3_auto_extension :: Ptr () -> IO Status

foreign import ccall "sqlite3.h sqlite3_reset_auto_extension"
  sqlite3_reset_auto_extension :: IO ()
-}
foreign import ccall "sqlite3.h sqlite3_blob_open"
  sqlite3_blob_open
          :: SQLite
          -> CString
          -> CString
          -> CString
          -> SQLiteInt64
          -> Bool
          -> Ptr SQLiteBLOB
          -> IO Status

foreign import ccall "sqlite3.h sqlite3_blob_close"
  sqlite3_blob_close :: SQLiteBLOB -> IO Status

foreign import ccall "sqlite3.h sqlite3_blob_bytes"
  sqlite3_blob_bytes :: SQLiteBLOB -> IO CInt

foreign import ccall "sqlite3.h sqlite3_blob_read"
  sqlite3_blob_read :: SQLiteBLOB -> Ptr () -> CInt -> CInt -> IO Status

foreign import ccall "sqlite3.h sqlite3_blob_write"
  sqlite3_blob_write :: SQLiteBLOB -> Ptr () -> CInt -> CInt -> IO Status
