{-# LANGUAGE PatternSignatures #-}
module Database.SQLite.VFS where

import Database.SQLite.VFS.Types
import Control.Monad
import Debug.Trace
import Prelude hiding (catch)
import System.IO
import System.IO.Error
import System.Directory
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

registerDirectoryVfs =
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
    f <- init_my_file zName
    poke my_file_ptr f
    createDirectoryIfMissing False name
    return 0

vdelete :: XDelete
vdelete _ zName syncDir =
 do name <- peekCString zName
    putStrLn ("Delete: " ++ name)
    putStrLn (" Sync: " ++ show syncDir)
    removeDirectoryRecursive name
    return 0

vaccess :: XAccess
vaccess _ zName flags =
 do name <- peekCString zName
    putStrLn ("Access: " ++ name)
    putStrLn (" Flags: " ++ show flags)
    perms <- getPermissions name
    return $ if      flags == sQLITE_ACCESS_EXISTS    then True
             else if flags == sQLITE_ACCESS_READ      then readable perms
             else if flags == sQLITE_ACCESS_READWRITE then readable perms &&
                                                           writable perms
             else False
  `catch` \ e -> if isDoesNotExistError e then return False else ioError e

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

init_my_file :: CString -> IO MySqliteFile
init_my_file zName = return MySqliteFile `ap` init_file `ap` return zName

init_file = SqliteFile `fmap` (new =<< init_io_methods)

init_io_methods :: IO SqliteIoMethods
init_io_methods =
  return SqliteIoMethods
    `ap` mkXClose vclose
    `ap` mkXRead  vread
    `ap` mkXWrite vwrite
    `ap` mkXTruncate vtruncate
    `ap` mkXSync vsync
    `ap` mkXFileSize vfilesize
    `ap` mkXLock vlock
    `ap` mkXUnlock vunlock
    `ap` mkXCheckReservedLock vcheckres
    `ap` mkXFileControl vfilecontrol
    `ap` mkXSectorSize vsectorsize
    `ap` mkXDeviceCharacteristics vdevchar

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
 do freeIoMethods . pMethods . myBaseFile =<< peek ptr
    return 0

getMyFilename :: Ptr MySqliteFile -> IO String
getMyFilename ptr = peekCString . myFilename =<< peek ptr

doRead ptr name i fn amt offset
  | amt <= 0 = return 0
  | otherwise = do
  let amt' = min (1024 - offset) amt
  got <- (withBinaryFile (name ++ "/" ++ show fn) ReadMode $ \ h ->
           do hSeek h AbsoluteSeek (fromIntegral offset)
              hGetBuf h (ptr `advancePtr` i) amt')
         `catch` \ _ -> return 0
  if got < amt' then return got else do
    got' <- doRead ptr name (i+got) (fn+1) (amt-got) 0
    return (got + got')

doWrite :: Ptr Word8 -> String -> Int -> Int -> Int -> Int -> IO ()
doWrite ptr name i fn amt offset
  | amt <= 0 = return ()
  | otherwise = do
  let amt' = min (1024 - offset) amt
  allocaArray 1024 $ \ (arr :: Ptr Word8) -> do
    (withBinaryFile (name ++ "/" ++ show fn) ReadMode $ \ h ->
      hGetBuf h arr 1024) `catch` \ _ -> return 0
    copyWord8Array (arr `advancePtr` offset) (ptr `advancePtr` i) amt'
    withBinaryFile (name ++ "/" ++ show fn) WriteMode $ \ h ->
      hPutBuf h arr 1024
  doWrite ptr name (i+amt') (fn+1) (amt-amt') 0

copyWord8Array :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
copyWord8Array = copyArray

vread :: XRead
vread p buffer amt offset =
 do name <- getMyFilename p
    let a = fromIntegral amt
        o = fromIntegral offset
    putStrLn ("Read: " ++ name)
    putStrLn (" amt: " ++ show amt)
    putStrLn (" offset: " ++ show offset)
    let firstNumber = o `div` 1024
    got <- doRead buffer name 0 firstNumber a (o - firstNumber * 1024)
    if got < a then return 522 else return 0

vwrite :: XWrite
vwrite p buffer amt offset =
 do name <- getMyFilename p
    let a = fromIntegral amt
        o = fromIntegral offset
    putStrLn "Write"
    putStrLn (" amt: " ++ show amt)
    putStrLn (" offset: " ++ show offset)
    let firstNumber = o `div` 1024
    doWrite buffer name 0 firstNumber a (o - firstNumber * 1024)
    return 0

vtruncate :: XTruncate
vtruncate p newsize =
 do name <- getMyFilename p
    putStrLn "Truncate"
    putStrLn (" newsize " ++ show newsize)
    return 0

vsync :: XSync
vsync _ flags = return 0

vfilesize :: XFileSize
vfilesize p pSize =
 do name <- getMyFilename p
    putStrLn ("Filesize: " ++ name)
    xs <- getDirectoryContents name `catch` \ _ -> return ["",""]
    poke pSize (fromIntegral (1024 * (length xs - 2)))
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
vsectorsize _ = return 1024

vdevchar :: XDeviceCharacteristics
vdevchar _ = return 1
