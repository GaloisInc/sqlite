{-# LANGUAGE ForeignFunctionInterface #-}

module Database.SQLite.VFS.Types where

import Control.Monad
import Foreign
import Foreign.C

#include "sqlite3.h"
#include "sqlite3-local.h"
#include <stddef.h>
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

type XOpen = Ptr SqliteVFS -> CString -> Ptr MySqliteFile -> CInt -> Ptr CInt
           -> IO CInt
type XDelete            = Ptr SqliteVFS -> CString -> CInt    -> IO CInt
type XAccess            = Ptr SqliteVFS -> CString -> SqliteAccessFlag -> IO Bool
type XGetTempname       = Ptr SqliteVFS -> CInt    -> CString -> IO CInt
type XFullPathname      = Ptr SqliteVFS -> CString -> CInt    -> CString -> IO CInt
type XDlOpen            = Ptr SqliteVFS -> CString -> IO ()
type XDlError           = Ptr SqliteVFS -> CInt    -> CString -> IO ()
type XDlSym             = Ptr SqliteVFS -> Ptr ()  -> CString -> IO ()
type XDlClose           = Ptr SqliteVFS -> Ptr ()  -> IO ()
type XRandomness        = Ptr SqliteVFS -> CInt    -> CString -> IO CInt
type XSleep             = Ptr SqliteVFS -> CInt    -> IO CInt
type XCurrentTime       = Ptr SqliteVFS -> Ptr CDouble -> IO CInt

foreign import ccall "wrapper" mkXOpen :: XOpen -> IO (FunPtr XOpen)
foreign import ccall "wrapper" mkXDelete :: XDelete -> IO (FunPtr XDelete)
foreign import ccall "wrapper" mkXAccess :: XAccess -> IO (FunPtr XAccess)
foreign import ccall "wrapper" mkXGetTempname ::
                                        XGetTempname -> IO (FunPtr XGetTempname)
foreign import ccall "wrapper" mkXFullPathname ::
                                      XFullPathname -> IO (FunPtr XFullPathname)
foreign import ccall "wrapper" mkXDlOpen :: XDlOpen -> IO (FunPtr XDlOpen)
foreign import ccall "wrapper" mkXDlError :: XDlError -> IO (FunPtr XDlError)
foreign import ccall "wrapper" mkXDlSym :: XDlSym -> IO (FunPtr XDlSym)
foreign import ccall "wrapper" mkXDlClose :: XDlClose -> IO (FunPtr XDlClose)
foreign import ccall "wrapper" mkXRandomness ::
                                          XRandomness -> IO (FunPtr XRandomness)
foreign import ccall "wrapper" mkXSleep :: XSleep -> IO (FunPtr XSleep)
foreign import ccall "wrapper" mkXCurrentTime ::
                                        XCurrentTime -> IO (FunPtr XCurrentTime)

data SqliteVFS = SqliteVFS
  { mxPathname          :: CInt
  , pNext               :: Ptr SqliteVFS
  , zName               :: CString
  , pAppData            :: Ptr ()
  , xOpen               :: FunPtr XOpen
  , xDelete             :: FunPtr XDelete
  , xAccess             :: FunPtr XAccess
  , xGetTempname        :: FunPtr XGetTempname
  , xFullPathname       :: FunPtr XFullPathname
  , xDlOpen             :: FunPtr XDlOpen
  , xDlError            :: FunPtr XDlError
  , xDlSym              :: FunPtr XDlSym
  , xDlClose            :: FunPtr XDlClose
  , xRandomness         :: FunPtr XRandomness
  , xSleep              :: FunPtr XSleep
  , xCurrentTime        :: FunPtr XCurrentTime
  }


instance Storable SqliteVFS where
  sizeOf _              = #size sqlite3_vfs
  alignment _           = #alignment sqlite3_vfs
  peek ptr              = return SqliteVFS
                          `ap` (#peek sqlite3_vfs, mxPathname) ptr
                          `ap` (#peek sqlite3_vfs, pNext) ptr
                          `ap` (#peek sqlite3_vfs, zName) ptr
                          `ap` (#peek sqlite3_vfs, pAppData) ptr
                          `ap` (#peek sqlite3_vfs, xOpen) ptr
                          `ap` (#peek sqlite3_vfs, xDelete) ptr
                          `ap` (#peek sqlite3_vfs, xAccess) ptr
                          `ap` (#peek sqlite3_vfs, xGetTempname) ptr
                          `ap` (#peek sqlite3_vfs, xFullPathname) ptr
                          `ap` (#peek sqlite3_vfs, xDlOpen) ptr
                          `ap` (#peek sqlite3_vfs, xDlError) ptr
                          `ap` (#peek sqlite3_vfs, xDlSym) ptr
                          `ap` (#peek sqlite3_vfs, xDlClose) ptr
                          `ap` (#peek sqlite3_vfs, xRandomness) ptr
                          `ap` (#peek sqlite3_vfs, xSleep) ptr
                          `ap` (#peek sqlite3_vfs, xCurrentTime) ptr

  poke ptr s= do (#poke sqlite3_vfs, iVersion) ptr (1 :: CInt)
                 (#poke sqlite3_vfs, szOsFile) ptr (sizeOf (undefined::MySqliteFile))
                 (#poke sqlite3_vfs, mxPathname) ptr (mxPathname s)
                 (#poke sqlite3_vfs, pNext) ptr (pNext s)
                 (#poke sqlite3_vfs, zName) ptr (zName s)
                 (#poke sqlite3_vfs, pAppData) ptr (pAppData s)
                 (#poke sqlite3_vfs, xOpen) ptr (xOpen s)
                 (#poke sqlite3_vfs, xDelete) ptr (xDelete s)
                 (#poke sqlite3_vfs, xAccess) ptr (xAccess s)
                 (#poke sqlite3_vfs, xGetTempname) ptr (xGetTempname s)
                 (#poke sqlite3_vfs, xFullPathname) ptr (xFullPathname s)
                 (#poke sqlite3_vfs, xDlOpen) ptr (xDlOpen s)
                 (#poke sqlite3_vfs, xDlError) ptr (xDlError s)
                 (#poke sqlite3_vfs, xDlSym) ptr (xDlSym s)
                 (#poke sqlite3_vfs, xDlClose) ptr (xDlClose s)
                 (#poke sqlite3_vfs, xRandomness) ptr (xRandomness s)
                 (#poke sqlite3_vfs, xSleep) ptr (xSleep s)
                 (#poke sqlite3_vfs, xCurrentTime) ptr (xCurrentTime s)

data MySqliteFile = MySqliteFile
        { myBaseFile :: SqliteFile
        , myFilename :: CString
        }

instance Storable MySqliteFile where
  sizeOf _                      = #size my_sqlite3_file
  alignment _                   = #alignment my_sqlite3_file
  peek ptr                      = return MySqliteFile
                                 `ap` (#peek my_sqlite3_file, base_file) ptr
                                 `ap` (#peek my_sqlite3_file, zFilename) ptr
  poke ptr s          = do (#poke my_sqlite3_file, base_file) ptr (myBaseFile s)
                           (#poke my_sqlite3_file, zFilename) ptr (myFilename s)

data SqliteFile = SqliteFile
        { pMethods :: Ptr SqliteIoMethods }

instance Storable SqliteFile where
  sizeOf _                      = #size sqlite3_file
  alignment _                   = #alignment sqlite3_file
  peek ptr                      = return SqliteFile
                                  `ap` (#peek sqlite3_file, pMethods) ptr
  poke ptr s                  = (#poke sqlite3_file, pMethods) ptr (pMethods s)

type XClose             = Ptr MySqliteFile -> IO CInt
type XRead              = Ptr MySqliteFile -> Ptr Word8 -> CInt -> Int64 -> IO CInt
type XWrite             = Ptr MySqliteFile -> Ptr Word8 -> CInt -> Int64 -> IO CInt
type XTruncate          = Ptr MySqliteFile -> Int64 -> IO CInt
type XSync              = Ptr MySqliteFile -> CInt -> IO CInt
type XFileSize          = Ptr MySqliteFile -> Ptr Int64 -> IO CInt
type XLock              = Ptr MySqliteFile -> SqliteLockFlag -> IO CInt
type XUnlock            = Ptr MySqliteFile -> SqliteLockFlag -> IO CInt
type XCheckReservedLock = Ptr MySqliteFile -> IO CInt
type XFileControl       = Ptr MySqliteFile -> CInt -> Ptr () -> IO CInt
type XSectorSize        = Ptr MySqliteFile -> IO CInt
type XDeviceCharacteristics = Ptr MySqliteFile -> IO CInt

foreign import ccall "wrapper" mkXClose :: XClose -> IO (FunPtr XClose)
foreign import ccall "wrapper" mkXRead  :: XRead  -> IO (FunPtr XRead)
foreign import ccall "wrapper" mkXWrite :: XWrite -> IO (FunPtr XWrite)
foreign import ccall "wrapper" mkXTruncate :: XTruncate -> IO (FunPtr XTruncate)
foreign import ccall "wrapper" mkXSync  :: XSync  -> IO (FunPtr XSync)
foreign import ccall "wrapper" mkXFileSize :: XFileSize -> IO (FunPtr XFileSize)
foreign import ccall "wrapper" mkXLock  :: XLock  -> IO (FunPtr XLock)
foreign import ccall "wrapper" mkXUnlock :: XUnlock -> IO (FunPtr XUnlock)
foreign import ccall "wrapper" mkXCheckReservedLock ::
                   XCheckReservedLock -> IO (FunPtr XCheckReservedLock)
foreign import ccall "wrapper" mkXFileControl ::
                                 XFileControl -> IO (FunPtr XFileControl)
foreign import ccall "wrapper" mkXSectorSize ::
                                 XSectorSize -> IO (FunPtr XSectorSize)
foreign import ccall "wrapper" mkXDeviceCharacteristics ::
                   XDeviceCharacteristics -> IO (FunPtr XDeviceCharacteristics)

data SqliteIoMethods = SqliteIoMethods
        { xClose                 :: FunPtr XClose
        , xRead                  :: FunPtr XRead
        , xWrite                 :: FunPtr XWrite
        , xTruncate              :: FunPtr XTruncate
        , xSync                  :: FunPtr XSync
        , xFileSize              :: FunPtr XFileSize
        , xLock                  :: FunPtr XLock
        , xUnlock                :: FunPtr XUnlock
        , xCheckReservedLock     :: FunPtr XCheckReservedLock
        , xFileControl           :: FunPtr XFileControl
        , xSectorSize            :: FunPtr XSectorSize
        , xDeviceCharacteristics :: FunPtr XDeviceCharacteristics
        }

instance Storable SqliteIoMethods where
  sizeOf _                      = #size sqlite3_io_methods
  alignment _                   = #alignment sqlite3_io_methods
  peek ptr                      = return SqliteIoMethods
                   `ap` (#peek sqlite3_io_methods, xClose) ptr
                   `ap` (#peek sqlite3_io_methods, xRead) ptr
                   `ap` (#peek sqlite3_io_methods, xWrite) ptr
                   `ap` (#peek sqlite3_io_methods, xTruncate) ptr
                   `ap` (#peek sqlite3_io_methods, xSync) ptr
                   `ap` (#peek sqlite3_io_methods, xFileSize) ptr
                   `ap` (#peek sqlite3_io_methods, xLock) ptr
                   `ap` (#peek sqlite3_io_methods, xUnlock) ptr
                   `ap` (#peek sqlite3_io_methods, xCheckReservedLock) ptr
                   `ap` (#peek sqlite3_io_methods, xFileControl) ptr
                   `ap` (#peek sqlite3_io_methods, xSectorSize) ptr
                   `ap` (#peek sqlite3_io_methods, xDeviceCharacteristics) ptr
  poke ptr s = do
    (#poke sqlite3_io_methods, xClose) ptr (xClose s)
    (#poke sqlite3_io_methods, xRead) ptr (xRead s)
    (#poke sqlite3_io_methods, xWrite) ptr (xWrite s)
    (#poke sqlite3_io_methods, xTruncate) ptr (xTruncate s)
    (#poke sqlite3_io_methods, xSync) ptr (xSync s)
    (#poke sqlite3_io_methods, xFileSize) ptr (xFileSize s)
    (#poke sqlite3_io_methods, xLock) ptr (xLock s)
    (#poke sqlite3_io_methods, xUnlock) ptr (xUnlock s)
    (#poke sqlite3_io_methods, xCheckReservedLock) ptr (xCheckReservedLock s)
    (#poke sqlite3_io_methods, xFileControl) ptr (xFileControl s)
    (#poke sqlite3_io_methods, xSectorSize) ptr (xSectorSize s)
    (#poke sqlite3_io_methods, xDeviceCharacteristics) ptr
                                                     (xDeviceCharacteristics s)

type SqliteLockFlag = CInt
sQLITE_LOCK_NONE, sQLITE_LOCK_SHARED, sQLITE_LOCK_RESERVED,
  sQLITE_LOCK_PENDING, sQLITE_LOCK_EXCLUSIVE :: SqliteLockFlag
sQLITE_LOCK_NONE        = (#const SQLITE_LOCK_NONE)
sQLITE_LOCK_SHARED      = (#const SQLITE_LOCK_SHARED)
sQLITE_LOCK_RESERVED    = (#const SQLITE_LOCK_RESERVED)
sQLITE_LOCK_PENDING     = (#const SQLITE_LOCK_PENDING)
sQLITE_LOCK_EXCLUSIVE   = (#const SQLITE_LOCK_EXCLUSIVE)

type SqliteAccessFlag = CInt
sQLITE_ACCESS_EXISTS, sQLITE_ACCESS_READWRITE, sQLITE_ACCESS_READ
                                                            :: SqliteAccessFlag
sQLITE_ACCESS_EXISTS    = (#const SQLITE_ACCESS_EXISTS)
sQLITE_ACCESS_READ      = (#const SQLITE_ACCESS_READ)
sQLITE_ACCESS_READWRITE = (#const SQLITE_ACCESS_READWRITE)
