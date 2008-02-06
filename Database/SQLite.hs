--------------------------------------------------------------------
-- |
-- Module    :  Database.SQLite
-- Copyright :  (c) Galois, Inc. 2007
-- License   :  BSD3
--
-- Maintainer:  docserver-dev-team@galois.com
-- Stability :  provisional
-- Portability: portable
--
-- A Haskell binding to the sqlite3 database.
-- See:
--
-- * <http://www.sqlite.org/>
--
-- for more information.
--
-- The api is documented at:
--
-- * <http://www.sqlite.org/c3ref/funclist.html>
--

module Database.SQLite
       ( module Database.SQLite.Base
       , module Database.SQLite.Types
       , module Database.SQL.Types

       -- * Opening and closing a database
       , openConnection   -- :: String -> IO SQLite
       , closeConnection  -- :: SQLite -> IO ()

       -- * Executing SQL queries on the database
       , execStatement        -- :: SQLite -> String -> IO (Either String Row)
       , execStatement_       -- :: SQLite -> String -> IO (Maybe String)
       , execParamStatement   -- :: SQLite -> String -> [(String,Value)]
                              --                     -> IO (Either String Row)
       , execParamStatement_  -- :: SQLite -> String -> [(String,Value)]
                              --                     -> IO (Maybe String)

       -- * Basic insertion operations
       , insertRow
       , defineTable
       , getLastRowID
       , Row
       , Value(..)

       ) where

import Database.SQLite.Types
import Database.SQLite.Base
import Database.SQL.Types

import Foreign.Marshal
import Foreign.C
import Foreign.C.String (newCStringLen, peekCString)
import Foreign.Storable
import Foreign.Ptr
-- import Data.IORef
import Data.List
import Data.Char ( isDigit )
import Control.Monad (when)
import qualified Codec.Binary.UTF8.String as UTF8

------------------------------------------------------------------------

-- | Open a new database connection, whose name is given
-- by the 'dbName' argument. A sqlite3 handle is returned.
--
-- An exception is thrown if the database could not be opened.
--
openConnection :: String -> IO SQLite
openConnection dbName = do
  ptr <- malloc
  st  <- withCString dbName $ \ c_dbName ->
                sqlite3_open c_dbName ptr
  case st of
    0 -> peek ptr
    _ -> fail ("openDatabase: failed to open " ++ show st)

-- | Close a database connection.
-- Destroys the SQLite value associated with a database, closes
-- all open files relating to the database, and releases all resources.
--
closeConnection :: SQLite -> IO ()
closeConnection h = sqlite3_close h >> return ()

------------------------------------------------------------------------
-- Adding data

type Row = [(ColumnName,String)]

-- | Define a new table, populated from 'tab' in the database.
--
defineTable :: SQLite -> SQLTable -> IO ()
defineTable h tab = do
   failOnLeft "defineTable" $ execStatement h (createTable tab)
   return ()
 where
  createTable t =
    "CREATE TABLE " ++ toSQLString (tabName t) ++
    tupled (map toCols (tabColumns t)) ++ ";"

  toCols col =
    toSQLString (colName col) ++ " " ++ showType (colType col) ++
    ' ':unwords (map showClause (colClauses col))

-- | Insert a row into the table 'tab'.
insertRow :: SQLite -> TableName -> Row -> IO ()
insertRow h tab cs = do
   let stmt = ("INSERT INTO " ++ tab ++
               tupled (toVals fst) ++ " VALUES " ++
               tupled (toVals (quote.snd)) ++ ";")
   failOnLeft "insertRow" $ execStatement h stmt
   return ()
  where
   toVals f = map (toVal f) cs
   toVal f p = f p -- ($ f)

   quote "" = "''"
   quote nm@(x:_)
    | isDigit x = nm
    | otherwise = '\'':toSQLString nm ++ "'"


-- | Return the rowid (as an Integer) of the most recent
-- successful INSERT into the database.
--
getLastRowID :: SQLite -> IO Integer
getLastRowID h = do
  v <- sqlite3_last_insert_rowid h
  return (fromIntegral v)

------------------------------------------------------------------------
-- Executing queries

failOnLeft :: String -> IO (Either String a) -> IO a
failOnLeft loc act = do
   r <- act
   case r of
     Right v -> return v
     Left e  -> fail (loc ++ ": failed - " ++ e)


data Value
  = Double Double
  | Int CInt
  | Int64 SQLiteInt64
  | Text String
  | Null

foreign import ccall "stdlib.h &free"
  p_free :: FunPtr (Ptr a -> IO ())


-- | Sets the value of a parameter in a statement.
-- Perofrms UTF8 encoding.
bindValue :: SQLiteStmt -> String -> Value -> IO Status
bindValue stmt key value =
  withCString (UTF8.encodeString key) $ \ckey ->
  sqlite3_bind_parameter_index stmt ckey >>= \ix ->
  case value of
    Text txt ->
      do (cptr,len) <- newCStringLen (UTF8.encodeString txt)
         res <- sqlite3_bind_text stmt ix cptr (fromIntegral len) p_free
         -- XXX: should we be doing this?
         when (res /= sQLITE_OK) (free cptr)
         return res
    Null     -> sqlite3_bind_null stmt ix
    Int x    -> sqlite3_bind_int stmt ix x
    Int64 x  -> sqlite3_bind_int64 stmt ix x
    Double x -> sqlite3_bind_double stmt ix x


-- | Called when we know that an error has occured.
to_error :: SQLite -> IO (Either String a)
to_error db = Left `fmap` (peekCString =<< sqlite3_errmsg db)



-- | Prepare and execute a parameterized statment, ignoring the result.
-- See also 'execParamStatement'.
execParamStatement_ :: SQLite -> String -> [(String,Value)] -> IO (Maybe String)
execParamStatement_ db q ps =
  do res <- execParamStatement db q ps
     case res of
       Right {} -> return Nothing
       Left err -> return (Just err)


-- | Prepare and execute a parameterized statment.
-- Statement parameter names start with a colon (for example, @:col_id@).
-- Note that for the moment, column names should not contain \0
-- characters because that part of the column name will be ignored.
execParamStatement :: SQLite -> String -> [(String,Value)]
                   -> IO (Either String [Row])
execParamStatement db query params =
  alloca $ \stmt_ptr ->
  alloca $ \pzTail ->
  let encoded = UTF8.encodeString query in
  withCString encoded $ \zSql ->
    do let nByte = fromIntegral (length encoded)
       res <- sqlite3_prepare db zSql nByte stmt_ptr pzTail
       if res /= sQLITE_OK then to_error db else
         do stmt <- peek stmt_ptr
            ok <- bind_all stmt params
            if ok then recv_rows stmt else to_error db

  where
  bind_all _ [] = return True
  bind_all stmt ((k,v):xs) =
    do res <- bindValue stmt k v
       if res == sQLITE_OK then bind_all stmt xs else return False

  recv_rows stmt =
    do col_num <- sqlite3_column_count stmt
       let cols = [0..col_num-1]
       -- Note: column names should not contain \0 characters
       names <- mapM peekCString =<<
                        mapM (sqlite3_column_name stmt) cols
       let decoded_names = map UTF8.decodeString names
       get_rows stmt cols decoded_names []

  get_rows stmt cols col_names rows =
    do res <- sqlite3_step stmt
       case () of
         _ | res == sQLITE_ROW ->
           do let get_val n = do ptr <- sqlite3_column_text stmt n
                                 bytes <- sqlite3_column_bytes stmt n
                                 peekCStringLen (ptr,fromIntegral bytes)
              txts <- mapM get_val cols
              let row = zip col_names (map UTF8.decodeString txts)
              get_rows stmt cols col_names (row:rows)
           | res == sQLITE_DONE -> do sqlite3_finalize stmt
                                      return (Right (reverse rows))
           | otherwise -> sqlite3_finalize stmt >> to_error db

-- | Evaluate the SQL statement specified by 'sqlStmt'
execStatement :: SQLite -> String -> IO (Either String [Row])
execStatement db s = execParamStatement db s []

-- | Rerturns an error, or 'Nothing' if everything was OK.
execStatement_ :: SQLite -> String -> IO (Maybe String)
execStatement_ db s = execParamStatement_ db s []

{-
-- | Evaluate the SQL statement specified by 'sqlStmt'
-- NOTE: At the moment this does not do UTF8 encoding.
execStatement :: SQLite -> String -> IO (Either String [Row])
execStatement h sqlStmt = do
 alloca $ \ p_errMsg ->
  withCString sqlStmt $ \ c_sqlStmt -> do
    m_rows <- newIORef []
    hdlr <- mkExecHandler (execHandler m_rows)
    st   <- sqlite3_exec h c_sqlStmt hdlr nullPtr p_errMsg
    case st of
      0 -> do
        ls <- readIORef m_rows
        return (Right (reverse ls))
      _x -> do
        pstr <- peek p_errMsg
        err <- peekCString pstr
        return (Left err)
 where
  execHandler ref _unused cols pCols pColNames = do
     let getStr ptr i = do
           cstr <- peekElemOff ptr i
	   if cstr == nullPtr
	    then return ""
	    else peekCString cstr
     vs <- mapM (getStr pCols) [0..(fromIntegral cols - 1)]
     cs <- mapM (getStr pColNames) [0..(fromIntegral cols - 1)]
     modifyIORef ref (\ ols -> (zip cs vs):ols)
     return 0
-}

tupled :: [String] -> String
tupled xs = "(" ++ concat (intersperse ", " xs) ++ ")"

