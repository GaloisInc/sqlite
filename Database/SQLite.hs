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
       , execStatements
       , execStatements_
       , execParamStatements
       , execParamStatements_

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
import Control.Monad ((<=<),when,unless)
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
   failOnLeft "defineTable" $ execStatements h (createTable tab)
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
   failOnLeft "insertRow" $ execStatements h stmt
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
  withCString (UTF8.encodeString key)      $ \ckey ->
  sqlite3_bind_parameter_index stmt ckey >>= \ix ->
  ensure (ix > 0)  (return sQLITE_OK)      $
  case value of
    Text txt ->
      do (cptr,len) <- newCStringLen (UTF8.encodeString txt)
         res <- sqlite3_bind_text stmt ix cptr (fromIntegral len) p_free
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
execParamStatements_ :: SQLite -> String -> [(String,Value)] -> IO (Maybe String)
execParamStatements_ db q ps =
  do res <- execParamStatements db q ps
     case res of
       Right {} -> return Nothing
       Left err -> return (Just err)


-- | Prepare and execute a parameterized statment.
-- Statement parameter names start with a colon (for example, @:col_id@).
-- Note that for the moment, column names should not contain \0
-- characters because that part of the column name will be ignored.
execParamStatements :: SQLite -> String -> [(String,Value)]
                   -> IO (Either String [[Row]])
execParamStatements db query params =
  alloca $ \stmt_ptr ->
  alloca $ \pzTail ->
  let encoded = UTF8.encodeString query in
  withCString encoded $ \zSql -> do
    res <- prepare_loop stmt_ptr pzTail zSql

    stmt <- peek stmt_ptr
    unless (isNullStmt stmt) (sqlite3_finalize stmt >> return ())

    return res

  where
  prepare_loop :: Ptr SQLiteStmt -> Ptr CString -> CString
               -> IO (Either String [[Row]])
  prepare_loop stmt_ptr pzTail zSql =
    let continue = prepare_loop stmt_ptr pzTail =<< peek pzTail in

    peek zSql                                    >>= \ sql ->
    ensure (sql /= 0)            (eReturn [])      $

    sqlite3_prepare db zSql (-1) stmt_ptr pzTail >>= \ res ->
    ensure (res == sQLITE_OK)    (to_error db)     $

    peek stmt_ptr                                >>= \ stmt ->
    ensure (not $ isNullStmt stmt)   continue      $

    bind_all stmt params                          >>
    recv_rows stmt                           `ebind` \ x ->
    continue                                 `ebind` \ xs ->
    eReturn (x:xs)


  bind_all stmt ps = mapM_ (uncurry $ bindValue stmt) ps

  recv_rows stmt =
    do col_num <- sqlite3_column_count stmt
       let cols = [0..col_num-1]
       -- Note: column names should not contain \0 characters
       names <- mapM (peekCString <=< sqlite3_column_name stmt) cols
       let decoded_names = map UTF8.decodeString names
       get_rows stmt cols decoded_names []

  get_rows stmt cols col_names rows =
    sqlite3_step stmt >>= \ step_res ->
    if step_res == sQLITE_ROW then do
      txts <- mapM (get_val stmt) cols
      let row = zip col_names txts
      get_rows stmt cols col_names (row:rows)
    else
      sqlite3_finalize stmt                      >>= \ final_res ->
      ensure (final_res == sQLITE_OK) (to_error db) $
      eReturn (reverse rows)

  get_val stmt n =
   do ptr   <- sqlite3_column_text stmt n
      bytes <- sqlite3_column_bytes stmt n
      str   <- peekCStringLen (ptr,fromIntegral bytes)
      return (UTF8.decodeString str)

-- | Evaluate the SQL statement specified by 'sqlStmt'
execStatements :: SQLite -> String -> IO (Either String [[Row]])
execStatements db s = execParamStatements db s []

-- | Returns an error, or 'Nothing' if everything was OK.
execStatements_ :: SQLite -> String -> IO (Maybe String)
execStatements_ db sqlStmt =
  let sqlStmt1 = UTF8.encodeString sqlStmt in
  withCString sqlStmt1                                 $ \ c_sqlStmt ->
  sqlite3_exec db c_sqlStmt noCallback nullPtr nullPtr >>= \ st ->
  if st == sQLITE_OK
    then return Nothing
    else fmap Just . peekCString =<< sqlite3_errmsg db

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

infixl 1 `ebind`
ebind :: Monad m => m (Either e a) -> (a -> m (Either e b)) -> m (Either e b)
m `ebind` f = do x <- m
                 case x of Left e -> return $ Left e
                           Right r -> f r

eReturn :: Monad m => a -> m (Either e a)
eReturn x = return $ Right x

ensure :: Bool -> a -> a -> a
ensure p t e = if p then e else t

