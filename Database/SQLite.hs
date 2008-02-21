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
       , openConnection
       , closeConnection

       -- * Executing SQL queries on the database
       , execStatement
       , execStatement_
       , execParamStatement
       , execParamStatement_

       -- * Basic insertion operations
       , insertRow
       , defineTable
       , defineTableOpt
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
import Data.List
import Data.Int
import Data.Char ( isDigit )
import Data.ByteString (ByteString, packCStringLen, useAsCStringLen)
import Control.Monad ((<=<),when)
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

type Row a = [(ColumnName,a)]

defineTableOpt :: SQLite -> Bool -> SQLTable -> IO (Maybe String)
defineTableOpt h check tab = execStatement_ h (createTable tab)
 where
  opt = if check then " IF NOT EXISTS " else ""
  createTable t =
    "CREATE TABLE " ++ toSQLString (tabName t) ++ opt ++
    tupled (map toCols (tabColumns t)) ++ ";"

  toCols col =
    toSQLString (colName col) ++ " " ++ showType (colType col) ++
    ' ':unwords (map showClause (colClauses col))


-- | Define a new table, populated from 'tab' in the database.
--
defineTable :: SQLite -> SQLTable -> IO (Maybe String)
defineTable h tab = defineTableOpt h False tab

-- | Insert a row into the table 'tab'.
insertRow :: SQLite -> TableName -> Row String -> IO (Maybe String)
insertRow h tab cs = do
   let stmt = ("INSERT INTO " ++ tab ++
               tupled (toVals fst) ++ " VALUES " ++
               tupled (toVals (quote.snd)) ++ ";")
   execStatement_ h stmt
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

data Value
  = Double Double
  | Int    Int64
  | Text   String
  | Blob   ByteString
  | Null
  deriving Show

foreign import ccall "stdlib.h &free"
  p_free :: FunPtr (Ptr a -> IO ())


-- | Sets the value of a parameter in a statement.
-- Perofrms UTF8 encoding.
bindValue :: SQLiteStmt -> String -> Value -> IO Status
bindValue stmt key value =
  withCString (UTF8.encodeString key)      $ \ckey ->
  ensure (sqlite3_bind_parameter_index stmt ckey)
         (> 0)  (return sQLITE_OK)         $ \ix ->
  case value of
    Text txt ->
      do (cptr,len) <- newCStringLen (UTF8.encodeString txt)
         res <- sqlite3_bind_text stmt ix cptr (fromIntegral len) p_free
         when (res /= sQLITE_OK) (free cptr)
         return res
    Null     -> sqlite3_bind_null stmt ix
    Int x    -> sqlite3_bind_int64 stmt ix x
    Double x -> sqlite3_bind_double stmt ix x
    Blob   x -> useAsCStringLen x $ \ (ptr,bytes) ->
                sqlite3_bind_blob stmt ix (castPtr ptr)
                                  (fromIntegral bytes) nullFunPtr


-- | Called when we know that an error has occured.
to_error :: SQLite -> IO (Either String a)
to_error db = Left `fmap` (peekCString =<< sqlite3_errmsg db)



-- | Prepare and execute a parameterized statment, ignoring the result.
-- See also 'execParamStatement'.
execParamStatement_ :: SQLite -> String -> [(String,Value)] -> IO (Maybe String)
execParamStatement_ db q ps =
  either Just (const Nothing) `fmap`
    (execParamStatement db q ps :: IO (Either String [[Row ()]]))

-- | Prepare and execute a parameterized statment.
-- Statement parameter names start with a colon (for example, @:col_id@).
-- Note that for the moment, column names should not contain \0
-- characters because that part of the column name will be ignored.
execParamStatement :: SQLiteResult a => SQLite -> String -> [(String,Value)]
                   -> IO (Either String [[Row a]])
execParamStatement db query params =
  alloca $ \stmt_ptr ->
  alloca $ \pzTail ->
  let encoded = UTF8.encodeString query in
  withCString encoded $ \zSql -> do
    poke pzTail zSql
    prepare_loop stmt_ptr pzTail

  where
  prepare_loop stmt_ptr sqltxt_ptr = loop [] where
    loop xs =
      peek sqltxt_ptr >>= \ sqltxt ->
      ensure_ (peek sqltxt) (/= 0) (eReturn (reverse xs)) $

      ensure_ (sqlite3_prepare db sqltxt (-1) stmt_ptr sqltxt_ptr)
             (== sQLITE_OK) (to_error db)          $

      ensure (peek stmt_ptr)
             (not . isNullStmt)   (loop xs)        $ \ stmt ->

      recv_rows stmt `then_finalize` stmt    `ebind` \ x ->
      loop (x:xs)

  recv_rows stmt =
    do mapM_ (uncurry $ bindValue stmt) params
       col_num <- sqlite3_column_count stmt
       let cols = [0..col_num-1]
       -- Note: column names should not contain \0 characters
       names <- mapM (peekCString <=< sqlite3_column_name stmt) cols
       let decoded_names = map UTF8.decodeString names
       get_rows stmt cols decoded_names []

  get_rows stmt cols col_names rows = do
    res <- sqlite3_step stmt
    if res == sQLITE_ROW
      then do
        txts <- mapM (get_sqlite_val stmt) cols
        let row = zip col_names txts
        get_rows stmt cols col_names (row:rows)
      else if res == sQLITE_DONE
        then eReturn (reverse rows)
      else to_error db

  then_finalize m stmt = do
    e <- m
    sqlite3_finalize stmt
    case e of
      Left _ -> to_error db
      Right r -> return (Right r)

-- | Evaluate the SQL statement specified by 'sqlStmt'
execStatement :: SQLiteResult a
               => SQLite -> String -> IO (Either String [[Row a]])
execStatement db s = execParamStatement db s []

-- | Returns an error, or 'Nothing' if everything was OK.
execStatement_ :: SQLite -> String -> IO (Maybe String)
execStatement_ db sqlStmt =
  withCString (UTF8.encodeString sqlStmt)              $ \ c_sqlStmt ->
  sqlite3_exec db c_sqlStmt noCallback nullPtr nullPtr >>= \ st ->
  if st == sQLITE_OK
    then return Nothing
    else fmap Just . peekCString =<< sqlite3_errmsg db

tupled :: [String] -> String
tupled xs = "(" ++ concat (intersperse ", " xs) ++ ")"

infixl 1 `ebind`
ebind :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
m `ebind` f = do x <- m
                 case x of Left e -> return $ Left e
                           Right r -> f r

eReturn :: Monad m => a -> m (Either e a)
eReturn x = return $ Right x

ensure :: Monad m => m a -> (a -> Bool) -> m b -> (a -> m b) -> m b
ensure m p t f = m >>= \ x -> if p x then f x else t

ensure_ :: Monad m => m a -> (a -> Bool) -> m b -> m b -> m b
ensure_ m p t f = ensure m p t (const f)

class SQLiteResult a where
  get_sqlite_val :: SQLiteStmt -> CInt -> IO a

instance SQLiteResult String where
  get_sqlite_val = get_text_val

instance SQLiteResult () where
  get_sqlite_val _ _ = return ()

instance SQLiteResult Value where
  get_sqlite_val = get_val

get_text_val :: SQLiteStmt -> CInt -> IO String
get_text_val stmt n =
 do ptr   <- sqlite3_column_text stmt n
    bytes <- sqlite3_column_bytes stmt n
    str   <- peekCStringLen (ptr,fromIntegral bytes)
    return (UTF8.decodeString str)

get_val :: SQLiteStmt -> CInt -> IO Value
get_val stmt n =
 do val <- sqlite3_column_value stmt n
    typ <- sqlite3_value_type val
    case () of
     _ | typ == sQLITE_NULL    -> return Null
       | typ == sQLITE_INTEGER -> Int `fmap` sqlite3_value_int64 val
       | typ == sQLITE_FLOAT   -> Double `fmap` sqlite3_value_double val
       | typ == sQLITE_TEXT    ->
                      do ptr <- sqlite3_value_text val
                         bytes <- sqlite3_value_bytes val
                         str <- peekCStringLen (ptr,fromIntegral bytes)
                         return $ Text str
       | typ == sQLITE_BLOB    ->
                      do SQLiteBLOB ptr <- sqlite3_value_blob val
                         bytes <- sqlite3_value_bytes val
                         str <- packCStringLen (castPtr ptr, fromIntegral bytes)
                         return $ Blob str
       | otherwise -> error "get_val: unknown type"
