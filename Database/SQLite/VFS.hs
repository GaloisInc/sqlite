module Database.SQLite.VFS where

import Database.SQLite.VFS.Types
import Control.Monad
import Data.Array.MArray
import Data.Array.IO
import Data.IORef
import Foreign.Ptr
import Foreign.C
import Foreign
import Data.Time.Clock
import Data.Time.LocalTime
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)

type DataArray = IOUArray Int Word8

maxPathname :: CInt
maxPathname = 255

foreign import ccall "sqlite3.h sqlite3_vfs_register" sqliteVfsRegister ::
  Ptr SqliteVFS -> Bool -> IO CInt

foreign import ccall "sqlite3.h sqlite3_vfs_find" sqliteVfsFind ::
  CString -> IO (Ptr SqliteVFS)

foreign import ccall "sqlite3.h sqlite3_vfs_unregister" sqliteVfsUnregister ::
  Ptr SqliteVFS -> IO CInt

unregisterVFS name =
 do ptr <- withCString name sqliteVfsFind
    unless (ptr == nullPtr) $
     do sqliteVfsUnregister ptr
        vfs <- peek ptr
        free (zName vfs)
        freeHaskellFunPtr (xOpen vfs)
        freeHaskellFunPtr (xDelete vfs)
        freeHaskellFunPtr (xAccess vfs)
        freeHaskellFunPtr (xGetTempname vfs)
        freeHaskellFunPtr (xFullPathname vfs)
        freeHaskellFunPtr (xDlOpen vfs)
        freeHaskellFunPtr (xDlError vfs)
        freeHaskellFunPtr (xDlSym vfs)
        freeHaskellFunPtr (xDlClose vfs)
        freeHaskellFunPtr (xRandomness vfs)
        freeHaskellFunPtr (xSleep vfs)
        freeHaskellFunPtr (xCurrentTime vfs)
        free ptr

register_array_vfs =
 do vfs <- new =<< return (SqliteVFS maxPathname nullPtr)
                   `ap` newCString "filebased"
                   `ap` return nullPtr
                   `ap` mkXOpen   vopen
                   `ap` mkXDelete vdelete
                   `ap` mkXAccess vaccess
                   `ap` mkXGetTempname vgettempname
                   `ap` mkXFullPathname vfullpathname
                   `ap` mkXDlOpen vdlopen
                   `ap` mkXDlError vdlerror
                   `ap` mkXDlSym vdlsym
                   `ap` mkXDlClose vdlclose
                   `ap` mkXRandomness vrandomness
                   `ap` mkXSleep vsleep
                   `ap` mkXCurrentTime vcurrenttime
    sqliteVfsRegister vfs True

vopen :: XOpen
vopen _ zName my_file_ptr flags pOutFlags =
 do name <- peekCString zName
    putStrLn ("Open: " ++ name)
    f <- init_my_file
    poke my_file_ptr f
    return 0

vdelete :: XDelete
vdelete _ zName syncDir =
 do name <- peekCString zName
    putStrLn ("Delete: " ++ name)
    putStrLn (" Sync: " ++ show syncDir)
    return 0

vaccess :: XAccess
vaccess _ zName flags =
 do name <- peekCString zName
    putStrLn ("Access: " ++ name)
    putStrLn (" Flags: " ++ show flags)
    return 0

vgettempname :: XGetTempname
vgettempname _ nOut zOut =
 do i <- randomRIO (100::Int,10000)
    let name = show i
    putStrLn ("GetTempname: " ++ name)
    if length name <= fromIntegral nOut
      then do pokeCString zOut name
              return 0
      else return 1

vfullpathname :: XFullPathname
vfullpathname _ zName nOut zOut =
 do name <- peekCString zName
    putStrLn ("FullPathname: " ++ name)
    if length name <= fromIntegral nOut
      then do pokeCString zOut name
              return 0
      else return 1

vdlopen :: XDlOpen
vdlopen = error "xDlOpen"

vdlerror :: XDlError
vdlerror = error "xDlError"

vdlsym :: XDlSym
vdlsym = error "xDlSym"

vdlclose :: XDlClose
vdlclose = error "xDlClose"

vrandomness :: XRandomness
vrandomness _ nByte zOut =
 do putStrLn ("Randomness: " ++ show nByte)
    xs <- replicateM (fromIntegral nByte) (randomRIO (-128::Int,127))
    pokeArray zOut $ map toEnum xs
    return nByte

vsleep :: XSleep
vsleep _ us = threadDelay (fromIntegral us) >> return 0

vcurrenttime :: XCurrentTime
vcurrenttime _ ptr =
 do zone <- getCurrentTimeZone
    now  <- getCurrentTime
    let local = utcToLocalTime zone now
        ut1 = localTimeToUT1 0 local
        m   = getModJulianDate ut1
    poke ptr (fromRational m)
    return 0

pokeCString :: Ptr CChar -> String -> IO ()
pokeCString ptr xs = pokeArray0 0 ptr (map (toEnum . fromEnum) xs)

init_my_file = liftM MySqliteFile init_file

init_file = do
  arr <- Data.Array.MArray.newArray (0,-1) 0
  ref <- newIORef arr
  pmeth <- new =<< return SqliteIoMethods
    `ap` mkXClose vclose
    `ap` mkXRead  (vread ref)
    `ap` mkXWrite (vwrite ref)
    `ap` mkXTruncate (vtruncate ref)
    `ap` mkXSync vsync
    `ap` mkXFileSize (vfilesize ref)
    `ap` mkXLock vlock
    `ap` mkXUnlock vunlock
    `ap` mkXCheckReservedLock vcheckres
    `ap` mkXFileControl vfilecontrol
    `ap` mkXSectorSize vsectorsize
    `ap` mkXDeviceCharacteristics vdevchar
  return (SqliteFile pmeth)

freeIoMethods ptr =
 do s <- peek ptr
    freeHaskellFunPtr (xClose s)
    freeHaskellFunPtr (xRead s)
    freeHaskellFunPtr (xWrite s)
    freeHaskellFunPtr (xTruncate s)
    freeHaskellFunPtr (xSync s)
    freeHaskellFunPtr (xFileSize s)
    freeHaskellFunPtr (xLock s)
    freeHaskellFunPtr (xUnlock s)
    freeHaskellFunPtr (xCheckReservedLock s)
    freeHaskellFunPtr (xFileControl s)
    freeHaskellFunPtr (xSectorSize s)
    freeHaskellFunPtr (xDeviceCharacteristics s)
    free ptr


vclose :: XClose
vclose ptr =
 do f <- peek ptr
    freeIoMethods (pMethods (baseFile f))
    return 0

vread :: IORef DataArray -> XRead
vread ref _ buffer amt offset =
 do let a = fromIntegral amt
        o = fromIntegral offset
    putStrLn "Read"
    putStrLn (" amt: " ++ show amt)
    putStrLn (" offset: " ++ show offset)
    sz <- size ref
    arr <- readIORef ref
    forM_ [0..a-1] $ \ i ->
      if i + o >= sz then pokeByteOff buffer i (0 :: Word8)
        else pokeByteOff buffer i =<< readArray arr (i + o)
    return 0

vwrite :: IORef DataArray -> XWrite
vwrite ref _ buffer amt offset =
 do let a = fromIntegral amt
        o = fromIntegral offset
    putStrLn "Write"
    putStrLn (" amt: " ++ show amt)
    putStrLn (" offset: " ++ show offset)
    sz <- size ref
    when (sz < o + a) (resize ref (o + a))
    arr <- readIORef ref
    forM_ [0..a-1] $ \ i ->
      writeArray arr (i+o) =<< peekByteOff buffer i
    return 0

vtruncate :: IORef DataArray -> XTruncate
vtruncate ref _ newsize =
 do putStrLn "Truncate"
    putStrLn (" newsize " ++ show newsize)
    resize ref (fromIntegral newsize)
    return 0

vsync :: XSync
vsync _ flags = return 0

vfilesize :: IORef DataArray -> XFileSize
vfilesize ref _ pSize =
 do sz <- size ref
    putStrLn ("Filesize: " ++ show sz)
    poke pSize (fromIntegral sz)
    return 0

vlock :: XLock
vlock _ flag = return 0

vunlock :: XUnlock
vunlock _ flag = return 0

vcheckres :: XCheckReservedLock
vcheckres _ = return 0

vfilecontrol :: XFileControl
vfilecontrol _ op pArg = return 0

vsectorsize :: XSectorSize
vsectorsize _ = return 1

vdevchar :: XDeviceCharacteristics
vdevchar _ = return 1

resize :: IORef DataArray -> Int -> IO ()
resize ref newsize =
 writeIORef ref =<< newListArray (0,newsize-1) =<< getElems =<< readIORef ref

size :: IORef DataArray -> IO Int
size ref =
 do (x,y) <- getBounds =<< readIORef ref
    return (y - x + 1)
