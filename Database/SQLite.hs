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
       , execStatement    -- :: SQLite -> String -> IO ()

       -- * Basic insertion operations
       , insertRow
       , defineTable
       , getLastRowID
       , Row

       ) where

import Database.SQLite.Types
import Database.SQLite.Base
import Database.SQL.Types

import Foreign.Marshal
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Data.IORef
import Data.List
import Data.Char ( isDigit )

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
   failOnRight "defineTable" $ execStatement h (createTable tab)
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
   failOnRight "insertRow" $ execStatement h stmt
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

failOnRight :: String -> IO (Either a String) -> IO a
failOnRight loc act = do
   r <- act
   case r of
     Left v   -> return v
     Right e  -> fail (loc ++ ": failed - " ++ e)

-- | Evaluate the SQL statement specified by 'sqlStmt'
execStatement :: SQLite -> String -> IO (Either [Row] String)
execStatement h sqlStmt = do
 alloca $ \ p_errMsg ->
  withCString sqlStmt $ \ c_sqlStmt -> do
    m_rows <- newIORef []
    hdlr <- mkExecHandler (execHandler m_rows)
    st   <- sqlite3_exec h c_sqlStmt hdlr nullPtr p_errMsg
    case st of
      0 -> do
        ls <- readIORef m_rows
        return (Left (reverse ls))
      _x -> do
        pstr <- peek p_errMsg
        err <- peekCString pstr
        return (Right err)
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

tupled :: [String] -> String
tupled xs = "(" ++ concat (intersperse ", " xs) ++ ")"

