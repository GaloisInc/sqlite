module SQLite
       ( module SQLite.Base
       , module SQLite.Types
       , module DB.SQL.Types
       
       , openConnection   -- :: String -> IO SQLite
       , execStatement    -- :: SQLite -> String -> IO ()
       , closeConnection  -- :: SQLite -> IO ()
       , insertRow        -- :: SQLite -> TableName -> [(ColumnName, String)] -> IO ()
       , defineTable      -- :: SQLite -> SQLTable  -> IO ()
       
       ) where

import SQLite.Types
import SQLite.Base
import DB.SQL.Types

import Foreign.Marshal
import Foreign.C
import Foreign.Storable
import Foreign.Ptr
import Data.IORef
import Data.List

openConnection :: String -> IO SQLite
openConnection dbName = do
  ptr <- malloc
  st  <- withCString dbName $ \ c_dbName -> 
                sqlite3_open c_dbName ptr
  case st of
    0 -> peek ptr
    _ -> fail ("openDatabase: failed to open " ++ show st)
		
closeConnection :: SQLite -> IO ()
closeConnection h = sqlite3_close h >> return ()

type Row = [(ColumnName,String)]

defineTable :: SQLite
	    -> SQLTable
	    -> IO ()
defineTable h tab = do
   execStatement h (createTable tab)
   return ()
 where
  createTable t = 
    "CREATE TABLE " ++ toSQLString (tabName t) ++ 
    tupled (map toCols (tabColumns t))

  toCols col = 
    toSQLString (colName col) ++ " " ++ showType (colType col) ++ 
    concatMap showClause (colClauses col)

insertRow :: SQLite
	  -> TableName
	  -> [(ColumnName, String)]
	  -> IO ()
insertRow h tab cs = do
   execStatement h ("INSERT INTO " ++ tab ++ 
                    tupled (toVals fst) ++ " VALUES " ++
		    tupled (toVals snd))
   return ()
  where
   toVals f = map (toVal f) cs
   
   toVal f p = '\'':toSQLString (f p) ++ "'"
   
execStatement :: SQLite -> String -> IO (Maybe [Row])
execStatement h sqlStmt = 
 alloca $ \ p_errMsg -> 
  withCString sqlStmt $ \ c_sqlStmt -> do
    m_rows <- newIORef []
    hdlr <- mkExecHandler (execHandler m_rows)
    st   <- sqlite3_exec h c_sqlStmt hdlr nullPtr p_errMsg
    case st of
      0 -> do
        ls <- readIORef m_rows
	return (Just ls)
      _x -> return Nothing
 where
  execHandler ref _unused cols pCols pColNames = do
     vs <- mapM (\ i -> peekElemOff pCols i >>= peekCString) [0..(fromIntegral cols - 1)]
     cs <- mapM (\ i -> peekElemOff pColNames i >>= peekCString) [0..(fromIntegral cols - 1)]
     modifyIORef ref (\ ols -> (zip cs vs):ols)
     return 0

tupled :: [String] -> String
tupled xs = "(" ++ concat (intersperse ", " xs) ++ ")"

