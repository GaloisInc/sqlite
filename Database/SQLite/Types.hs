{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Module    :  Database.SQLite.Types
-- Copyright :  (c) Galois, Inc. 2007
-- License   :  BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  provisional
-- Portability: portable
--
-- Objects, types and constants used in the sqlite3 binding.
--
--------------------------------------------------------------------

module Database.SQLite.Types where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Data.Int
import Data.Word
import Data.Bits

type SQLiteInt64  = Int64
type SQLiteWord64 = Word64

------------------------------------------------------------------------
-- 'typed' pointers to various SQLite handles\/values.

-- | An open SQLite database object.
--
newtype SQLite              = SQLite              (Ptr ())   deriving Storable

-- | An instance of this object represent single SQL statements. This
-- object is variously known as a "prepared statement" or a "compiled
-- SQL statement" or simply as a "statement".
--
newtype SQLiteStmt          = SQLiteStmt          (Ptr ())   deriving Storable

-- | SQLite uses the sqlite3_value object to represent all values that
-- are or can be stored in a database table. SQLite uses dynamic typing
-- for the values it stores. Values stored in sqlite3_value objects can
-- be be integers, floating point values, strings, BLOBs, or NULL.
--
newtype SQLiteValue         = SQLiteValue         (Ptr ())   deriving Storable

-- | The context in which an SQL function executes is stored in an
-- sqlite3_context object. A pointer to an sqlite3_context object is
-- always first parameter to application-defined SQL functions.
--
newtype SQLiteContext       = SQLiteContext       (Ptr ())   deriving Storable

-- | A BLOB handle
newtype SQLiteBLOB          = SQLiteBLOB          (Ptr ())   deriving Storable

newtype SQLiteUTF16         = SQLiteUTF16         (Ptr ())   deriving Storable
newtype SQLiteContextBuffer = SQLiteContextBuffer (Ptr ())   deriving Storable
newtype SQLiteCallback a    = SQLiteCallback      (FunPtr a) deriving Storable

-- don't use it much, so leave it as a bare pointer.
type SQLiteCallbackUserData = Ptr ()

------------------------------------------------------------------------

-- | SQLite types
data SQLiteType
 = SQLiteInt
 | SQLiteFloat
 | SQLiteText
 | SQLiteBlob
 | SQLiteNull
   deriving ( Eq )

instance Enum SQLiteType where
  fromEnum e =
    case e of
      SQLiteInt   -> 1
      SQLiteFloat -> 2
      SQLiteText  -> 3
      SQLiteBlob  -> 4
      SQLiteNull  -> 5
  toEnum x =
    case x of
      1 -> SQLiteInt
      2 -> SQLiteFloat
      3 -> SQLiteText
      4 -> SQLiteBlob
      5 -> SQLiteNull
      _ -> error ("toEnum{SQLiteType}: unknown type tag " ++ show x)

------------------------------------------------------------------------

type Status = Word32

-- | SQLite C status codes.
-- 
-- * <http://www.sqlite.org/c3ref/c_abort.html>
-- 
sQLITE_OK :: Status
sQLITE_OK           = 0

-- error codes:
sQLITE_ERROR        :: Status
sQLITE_ERROR        = 1
sQLITE_INTERNAL     :: Status
sQLITE_INTERNAL     = 2
sQLITE_PERM         :: Status
sQLITE_PERM         = 3
sQLITE_ABORT        :: Status
sQLITE_ABORT        = 4
sQLITE_BUSY         :: Status
sQLITE_BUSY         = 5
sQLITE_LOCKED       :: Status
sQLITE_LOCKED       = 6
sQLITE_NOMEM        :: Status
sQLITE_NOMEM        = 7
sQLITE_READONLY     :: Status
sQLITE_READONLY     = 8
sQLITE_INTERRUPT    :: Status
sQLITE_INTERRUPT    = 9
sQLITE_IOERR        :: Status
sQLITE_IOERR        = 10
sQLITE_CORRUPT      :: Status
sQLITE_CORRUPT      = 11
sQLITE_NOTFOUND     :: Status
sQLITE_NOTFOUND     = 12
sQLITE_FULL         :: Status
sQLITE_FULL         = 13
sQLITE_CANTOPEN     :: Status
sQLITE_CANTOPEN     = 14
sQLITE_PROTOCOL     :: Status
sQLITE_PROTOCOL     = 15
sQLITE_EMPTY        :: Status
sQLITE_EMPTY        = 16
sQLITE_SCHEMA       :: Status
sQLITE_SCHEMA       = 17
sQLITE_TOOBIG       :: Status
sQLITE_TOOBIG       = 18
sQLITE_CONSTRAINT   :: Status
sQLITE_CONSTRAINT   = 19
sQLITE_MISMATCH     :: Status
sQLITE_MISMATCH     = 20
sQLITE_MISUSE       :: Status
sQLITE_MISUSE       = 21
sQLITE_NOLFS        :: Status
sQLITE_NOLFS        = 22
sQLITE_AUTH         :: Status
sQLITE_AUTH         = 23
sQLITE_FORMAT       :: Status
sQLITE_FORMAT       = 24
sQLITE_RANGE        :: Status
sQLITE_RANGE        = 25
sQLITE_NOTADB       :: Status
sQLITE_NOTADB       = 26
sQLITE_ROW          :: Status
sQLITE_ROW          = 100
sQLITE_DONE         :: Status
sQLITE_DONE         = 101

------------------------------------------------------------------------
-- | SQLite extended result codes:
--
-- * <http://www.sqlite.org/c3ref/c_ioerr_blocked.html>
--
sQLITE_IOERR_READ       :: Status
sQLITE_IOERR_READ        = sQLITE_IOERR + (1 `shiftL` 8)
sQLITE_IOERR_SHORT_READ :: Status
sQLITE_IOERR_SHORT_READ  = sQLITE_IOERR + (2 `shiftL` 8)
sQLITE_IOERR_WRITE      :: Status
sQLITE_IOERR_WRITE       = sQLITE_IOERR + (3 `shiftL` 8)
sQLITE_IOERR_FSYNC     :: Status
sQLITE_IOERR_FSYNC       = sQLITE_IOERR + (4 `shiftL` 8)
sQLITE_IOERR_DIR_FSYNC :: Status
sQLITE_IOERR_DIR_FSYNC   = sQLITE_IOERR + (5 `shiftL` 8)
sQLITE_IOERR_TRUNCATE  :: Status
sQLITE_IOERR_TRUNCATE    = sQLITE_IOERR + (6 `shiftL` 8)
sQLITE_IOERR_FSTAT     :: Status
sQLITE_IOERR_FSTAT       = sQLITE_IOERR + (7 `shiftL` 8)
sQLITE_IOERR_UNLOCK    :: Status
sQLITE_IOERR_UNLOCK      = sQLITE_IOERR + (8 `shiftL` 8)
sQLITE_IOERR_RDLOCK    :: Status
sQLITE_IOERR_RDLOCK      = sQLITE_IOERR + (9 `shiftL` 8)
sQLITE_IOERR_DELETE    :: Status
sQLITE_IOERR_DELETE      = sQLITE_IOERR + (10 `shiftL` 8)
sQLITE_IOERR_BLOCKED   :: Status
sQLITE_IOERR_BLOCKED     = sQLITE_IOERR + (11 `shiftL` 8)
sQLITE_IOERR_NOMEM     :: Status
sQLITE_IOERR_NOMEM       = sQLITE_IOERR + (12 `shiftL` 8)

------------------------------------------------------------------------

type OpenFlags = Word32

-- | SQLite flags for open operations.
--
-- * <http://www.sqlite.org/c3ref/c_open_create.html>
--
sQLITE_OPEN_READONLY        :: OpenFlags
sQLITE_OPEN_READONLY        = 0x00000001
sQLITE_OPEN_READWRITE       :: OpenFlags
sQLITE_OPEN_READWRITE       = 0x00000002
sQLITE_OPEN_CREATE          :: OpenFlags
sQLITE_OPEN_CREATE          = 0x00000004
sQLITE_OPEN_DELETEONCLOSE   :: OpenFlags
sQLITE_OPEN_DELETEONCLOSE   = 0x00000008
sQLITE_OPEN_EXCLUSIVE       :: OpenFlags
sQLITE_OPEN_EXCLUSIVE       = 0x00000010
sQLITE_OPEN_MAIN_DB         :: OpenFlags
sQLITE_OPEN_MAIN_DB         = 0x00000100
sQLITE_OPEN_TEMP_DB         :: OpenFlags
sQLITE_OPEN_TEMP_DB         = 0x00000200
sQLITE_OPEN_TRANSIENT_DB    :: OpenFlags
sQLITE_OPEN_TRANSIENT_DB    = 0x00000400
sQLITE_OPEN_MAIN_JOURNAL    :: OpenFlags
sQLITE_OPEN_MAIN_JOURNAL    = 0x00000800
sQLITE_OPEN_TEMP_JOURNAL    :: OpenFlags
sQLITE_OPEN_TEMP_JOURNAL    = 0x00001000
sQLITE_OPEN_SUBJOURNAL      :: OpenFlags
sQLITE_OPEN_SUBJOURNAL      = 0x00002000
sQLITE_OPEN_MASTER_JOURNAL  :: OpenFlags
sQLITE_OPEN_MASTER_JOURNAL  = 0x00004000

------------------------------------------------------------------------

type IOCap = Word32

-- | Device characteristics
--
-- * <http://www.sqlite.org/c3ref/c_iocap_atomic.html>
--
sQLITE_IOCAP_ATOMIC      :: IOCap
sQLITE_IOCAP_ATOMIC      = 0x00000001
sQLITE_IOCAP_ATOMIC512   :: IOCap
sQLITE_IOCAP_ATOMIC512   = 0x00000002
sQLITE_IOCAP_ATOMIC1K    :: IOCap
sQLITE_IOCAP_ATOMIC1K    = 0x00000004
sQLITE_IOCAP_ATOMIC2K    :: IOCap
sQLITE_IOCAP_ATOMIC2K    = 0x00000008
sQLITE_IOCAP_ATOMIC4K    :: IOCap
sQLITE_IOCAP_ATOMIC4K    = 0x00000010
sQLITE_IOCAP_ATOMIC8K    :: IOCap
sQLITE_IOCAP_ATOMIC8K    = 0x00000020
sQLITE_IOCAP_ATOMIC16K   :: IOCap
sQLITE_IOCAP_ATOMIC16K   = 0x00000040
sQLITE_IOCAP_ATOMIC32K   :: IOCap
sQLITE_IOCAP_ATOMIC32K   = 0x00000080
sQLITE_IOCAP_ATOMIC64K   :: IOCap
sQLITE_IOCAP_ATOMIC64K   = 0x00000100
sQLITE_IOCAP_SAFE_APPEND :: IOCap
sQLITE_IOCAP_SAFE_APPEND = 0x00000200
sQLITE_IOCAP_SEQUENTIAL  :: IOCap
sQLITE_IOCAP_SEQUENTIAL  = 0x00000400

------------------------------------------------------------------------

type LockFlag = Word32

-- | File locking levels
--
-- * <http://www.sqlite.org/c3ref/c_lock_exclusive.html>
--
sQLITE_LOCK_NONE      :: LockFlag
sQLITE_LOCK_NONE          = 0
sQLITE_LOCK_SHARED    :: LockFlag
sQLITE_LOCK_SHARED        = 1
sQLITE_LOCK_RESERVED  :: LockFlag
sQLITE_LOCK_RESERVED      = 2
sQLITE_LOCK_PENDING   :: LockFlag
sQLITE_LOCK_PENDING       = 3
sQLITE_LOCK_EXCLUSIVE :: LockFlag
sQLITE_LOCK_EXCLUSIVE     = 4

------------------------------------------------------------------------

type SyncFlag = Word32

-- | Synchronization flags
--
-- * <http://www.sqlite.org/c3ref/c_sync_dataonly.html>
--
sQLITE_SYNC_NORMAL :: SyncFlag
sQLITE_SYNC_NORMAL     =   0x00002
sQLITE_SYNC_FULL   :: SyncFlag
sQLITE_SYNC_FULL       =   0x00003
sQLITE_SYNC_DATAONLY:: SyncFlag
sQLITE_SYNC_DATAONLY   =   0x00010

------------------------------------------------------------------------

type AccessFlag = Word32

-- | xAccess methods
--
-- * <http://www.sqlite.org/c3ref/c_access_exists.html>
--
sQLITE_ACCESS_EXISTS    :: AccessFlag
sQLITE_ACCESS_EXISTS    = 0
sQLITE_ACCESS_READWRITE :: AccessFlag
sQLITE_ACCESS_READWRITE = 1
sQLITE_ACCESS_READ      :: AccessFlag
sQLITE_ACCESS_READ      = 2

------------------------------------------------------------------------

type AuthCode = Word32

-- | Authorizer Action Codes
--
-- * <http://www.sqlite.org/c3ref/c_alter_table.html>
--
sQLITE_COPY                 :: AuthCode
sQLITE_COPY                 =  0   -- No longer used
sQLITE_CREATE_INDEX         :: AuthCode
sQLITE_CREATE_INDEX         = 1    -- Index Name      Table Name
sQLITE_CREATE_TABLE         :: AuthCode
sQLITE_CREATE_TABLE         = 2    -- Table Name      NULL
sQLITE_CREATE_TEMP_INDEX    :: AuthCode
sQLITE_CREATE_TEMP_INDEX    = 3    -- Index Name      Table Name
sQLITE_CREATE_TEMP_TABLE    :: AuthCode
sQLITE_CREATE_TEMP_TABLE    = 4    -- Table Name      NULL
sQLITE_CREATE_TEMP_TRIGGER  :: AuthCode
sQLITE_CREATE_TEMP_TRIGGER  = 5    -- Trigger Name    Table Name
sQLITE_CREATE_TEMP_VIEW     :: AuthCode
sQLITE_CREATE_TEMP_VIEW     = 6    -- View Name       NULL
sQLITE_CREATE_TRIGGER       :: AuthCode
sQLITE_CREATE_TRIGGER       = 7    -- Trigger Name    Table Name
sQLITE_CREATE_VIEW          :: AuthCode
sQLITE_CREATE_VIEW          = 8    -- View Name       NULL
sQLITE_DELETE               :: AuthCode
sQLITE_DELETE               = 9    -- Table Name      NULL
sQLITE_DROP_INDEX           :: AuthCode
sQLITE_DROP_INDEX           = 10   -- Index Name      Table Name
sQLITE_DROP_TABLE           :: AuthCode
sQLITE_DROP_TABLE           = 11   -- Table Name      NULL
sQLITE_DROP_TEMP_INDEX      :: AuthCode
sQLITE_DROP_TEMP_INDEX      = 12   -- Index Name      Table Name
sQLITE_DROP_TEMP_TABLE      :: AuthCode
sQLITE_DROP_TEMP_TABLE      = 13   -- Table Name      NULL
sQLITE_DROP_TEMP_TRIGGER    :: AuthCode
sQLITE_DROP_TEMP_TRIGGER    = 14   -- Trigger Name    Table Name
sQLITE_DROP_TEMP_VIEW       :: AuthCode
sQLITE_DROP_TEMP_VIEW       = 15   -- View Name       NULL
sQLITE_DROP_TRIGGER         :: AuthCode
sQLITE_DROP_TRIGGER         = 16   -- Trigger Name    Table Name
sQLITE_DROP_VIEW            :: AuthCode
sQLITE_DROP_VIEW            = 17   -- View Name       NULL
sQLITE_INSERT               :: AuthCode
sQLITE_INSERT               = 18   -- Table Name      NULL
sQLITE_PRAGMA               :: AuthCode
sQLITE_PRAGMA               = 19   -- Pragma Name     1st arg or NULL
sQLITE_READ                 :: AuthCode
sQLITE_READ                 = 20   -- Table Name      Column Name
sQLITE_SELECT               :: AuthCode
sQLITE_SELECT               = 21   -- NULL            NULL
sQLITE_TRANSACTION          :: AuthCode
sQLITE_TRANSACTION          = 22   -- NULL            NULL
sQLITE_UPDATE               :: AuthCode
sQLITE_UPDATE               = 23   -- Table Name      Column Name
sQLITE_ATTACH               :: AuthCode
sQLITE_ATTACH               = 24   -- Filename        NULL
sQLITE_DETACH               :: AuthCode
sQLITE_DETACH               = 25   -- Database Name   NULL
sQLITE_ALTER_TABLE          :: AuthCode
sQLITE_ALTER_TABLE          = 26   -- Database Name   Table Name
sQLITE_REINDEX              :: AuthCode
sQLITE_REINDEX              = 27   -- Index Name      NULL
sQLITE_ANALYZE              :: AuthCode
sQLITE_ANALYZE              = 28   -- Table Name      NULL
sQLITE_CREATE_VTABLE        :: AuthCode
sQLITE_CREATE_VTABLE        = 29   -- Table Name      Module Name
sQLITE_DROP_VTABLE          :: AuthCode
sQLITE_DROP_VTABLE          = 30   -- Table Name      Module Name
sQLITE_FUNCTION             :: AuthCode
sQLITE_FUNCTION             = 31   -- Function Name   NULL

------------------------------------------------------------------------

type TextEncodeFlag = CInt

-- | Text encodings
--
-- * <http://www.sqlite.org/c3ref/c_any.html>
--
sQLITE_UTF8          :: TextEncodeFlag
sQLITE_UTF8          = 1
sQLITE_UTF16LE       :: TextEncodeFlag
sQLITE_UTF16LE       = 2
sQLITE_UTF16BE       :: TextEncodeFlag
sQLITE_UTF16BE       = 3
sQLITE_UTF16         :: TextEncodeFlag
sQLITE_UTF16         = 4
sQLITE_ANY           :: TextEncodeFlag
sQLITE_ANY           = 5
sQLITE_UTF16_ALIGNED :: TextEncodeFlag
sQLITE_UTF16_ALIGNED = 8

type FundamentalDatatype = CInt

-- | Fundamental datatypes
--
-- * <http://www.sqlite.org/c3ref/c_blob.html>
--
sQLITE_INTEGER       :: FundamentalDatatype
sQLITE_INTEGER       = 1
sQLITE_FLOAT         :: FundamentalDatatype
sQLITE_FLOAT         = 2
sQLITE_BLOB          :: FundamentalDatatype
sQLITE_BLOB          = 4
sQLITE_NULL          :: FundamentalDatatype
sQLITE_NULL          = 5
sQLITE_TEXT          :: FundamentalDatatype
sQLITE_TEXT          = 3

isNullStmt :: SQLiteStmt -> Bool
isNullStmt (SQLiteStmt p) = p == nullPtr

noCallback :: SQLiteCallback a
noCallback = SQLiteCallback nullFunPtr

freeCallback :: SQLiteCallback a -> IO ()
freeCallback (SQLiteCallback c) = freeHaskellFunPtr c
