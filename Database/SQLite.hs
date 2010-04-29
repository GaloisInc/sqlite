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

       , addRegexpSupport
       , RegexpHandler
       , withPrim
       , SQLiteHandle()
       , newSQLiteHandle

       -- * User-defined callback functions
       , IsValue(..)
       , IsFunctionHandler(..)
       , createFunction
       , createFunctionPrim
       , createAggregatePrim
       ) where

import Database.SQLite.Types
import Database.SQLite.Base
import Database.SQL.Types

import Foreign.Marshal
import Foreign.C
import Foreign.C.String (newCStringLen, peekCString)
import Foreign.Storable
import qualified Foreign.Concurrent as Conc
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.ForeignPtr
import Data.List
import Data.Int
import Data.Char ( isDigit )
import Data.ByteString (ByteString, packCStringLen, useAsCStringLen)
import Data.ByteString.Unsafe (unsafePackCStringLen, unsafeUseAsCStringLen)
import Control.Monad ((<=<),when)
import qualified Codec.Binary.UTF8.String as UTF8

------------------------------------------------------------------------

newtype SQLiteHandle = SQLiteHandle (ForeignPtr ())

addSQLiteHandleFinalizer :: SQLiteHandle -> IO () -> IO ()
addSQLiteHandleFinalizer (SQLiteHandle h) = Conc.addForeignPtrFinalizer h

newSQLiteHandle :: SQLite -> IO SQLiteHandle
newSQLiteHandle h@(SQLite p) = SQLiteHandle `fmap` Conc.newForeignPtr p close
  where close = sqlite3_close h >> return ()

-- | Open a new database connection, whose name is given
-- by the 'dbName' argument. A sqlite3 handle is returned.
--
-- An exception is thrown if the database could not be opened.
--
openConnection :: String -> IO SQLiteHandle
openConnection dbName =
  alloca $ \ptr -> do
  st  <- withCString dbName $ \ c_dbName ->
                sqlite3_open c_dbName ptr
  case st of
    0 -> do db <- peek ptr
            newSQLiteHandle db
    _ -> fail ("openDatabase: failed to open " ++ show st)


-- | Close a database connection.
-- Destroys the SQLite value associated with a database, closes
-- all open files relating to the database, and releases all resources.
--
closeConnection :: SQLiteHandle -> IO ()
closeConnection (SQLiteHandle h) = finalizeForeignPtr h

withPrim :: SQLiteHandle -> (SQLite -> IO a) -> IO a
withPrim (SQLiteHandle h) f = withForeignPtr h (f . SQLite)

------------------------------------------------------------------------
-- Adding data

type Row a = [(ColumnName,a)]

defineTableOpt :: SQLiteHandle -> Bool -> SQLTable -> IO (Maybe String)
defineTableOpt h check tab = execStatement_ h (createTable tab)
 where
  opt = if check then " IF NOT EXISTS " else ""
  createTable t = case t of
    Table{} -> "CREATE TABLE " ++ namePart ++ bodyPart
    VirtualTable{} -> "CREATE VIRTUAL TABLE " ++ namePart ++ " USING " ++ tabUsing t ++ bodyPart
    where
    namePart = opt ++ toSQLString (tabName t)
    bodyPart = tupled (map toCols (tabColumns t)) ++ ";"

  toCols col =
    toSQLString (colName col) ++ " " ++ showType (colType col) ++
    ' ':unwords (map showClause (colClauses col))


-- | Define a new table, populated from 'tab' in the database.
--
defineTable :: SQLiteHandle -> SQLTable -> IO (Maybe String)
defineTable h tab = defineTableOpt h False tab

-- | Insert a row into the table 'tab'.
insertRow :: SQLiteHandle -> TableName -> Row String -> IO (Maybe String)
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
getLastRowID :: SQLiteHandle -> IO Integer
getLastRowID h = withPrim h $ \ p -> do
  v <- sqlite3_last_insert_rowid p
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
execParamStatement_ :: SQLiteHandle -> String -> [(String,Value)]
                    -> IO (Maybe String)
execParamStatement_ db q ps =
  either Just (const Nothing) `fmap`
    (execParamStatement db q ps :: IO (Either String [[Row ()]]))

-- | Prepare and execute a parameterized statment.
-- Statement parameter names start with a colon (for example, @:col_id@).
-- Note that for the moment, column names should not contain \0
-- characters because that part of the column name will be ignored.
execParamStatement :: SQLiteResult a => SQLiteHandle -> String
                   -> [(String,Value)] -> IO (Either String [[Row a]])
execParamStatement h query params = withPrim h $ \ db ->
  alloca $ \stmt_ptr ->
  alloca $ \pzTail ->
  let encoded = UTF8.encodeString query in
  withCString encoded $ \zSql -> do
    poke pzTail zSql
    prepare_loop db stmt_ptr pzTail

  where
  prepare_loop db stmt_ptr sqltxt_ptr = loop [] where
    loop xs =
      peek sqltxt_ptr >>= \ sqltxt ->
      ensure_ (peek sqltxt) (/= 0) (eReturn (reverse xs)) $

      ensure_ (sqlite3_prepare db sqltxt (-1) stmt_ptr sqltxt_ptr)
             (== sQLITE_OK) (to_error db)          $

      ensure (peek stmt_ptr)
             (not . isNullStmt)   (loop xs)        $ \ stmt ->

      then_finalize db (recv_rows db stmt) stmt `ebind` \ x ->
      loop (x:xs)

  recv_rows db stmt =
    do mapM_ (uncurry $ bindValue stmt) params
       col_num <- sqlite3_column_count stmt
       let cols = [0..col_num-1]
       -- Note: column names should not contain \0 characters
       names <- mapM (peekCString <=< sqlite3_column_name stmt) cols
       let decoded_names = map UTF8.decodeString names
       get_rows db stmt cols decoded_names []

  get_rows db stmt cols col_names rows = do
    res <- sqlite3_step stmt
    if res == sQLITE_ROW
      then do
        txts <- mapM (get_sqlite_val stmt) cols
        let row = zip col_names txts
        get_rows db stmt cols col_names (row:rows)
      else if res == sQLITE_DONE
        then eReturn (reverse rows)
      else to_error db

  then_finalize db m stmt = do
    e <- m
    sqlite3_finalize stmt
    case e of
      Left _ -> to_error db
      Right r -> return (Right r)

-- | Evaluate the SQL statement specified by 'sqlStmt'
execStatement :: SQLiteResult a
               => SQLiteHandle -> String -> IO (Either String [[Row a]])
execStatement db s = execParamStatement db s []

-- | Returns an error, or 'Nothing' if everything was OK.
execStatement_ :: SQLiteHandle -> String -> IO (Maybe String)
execStatement_ h sqlStmt = withPrim h $ \ db ->
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
get_val stmt n = sqlite3_value_value =<< sqlite3_column_value stmt n

sqlite3_value_value :: SQLiteValue -> IO Value
sqlite3_value_value val =
 do typ <- sqlite3_value_type val
    case () of
     _ | typ == sQLITE_NULL    -> return Null
       | typ == sQLITE_INTEGER -> Int `fmap` sqlite3_value_int64 val
       | typ == sQLITE_FLOAT   -> Double `fmap` sqlite3_value_double val
       | typ == sQLITE_TEXT    ->
                     fmap Text . peekCStringLen =<< sqlite3_value_cstringlen val
       | typ == sQLITE_BLOB    ->
                      do SQLiteBLOB ptr <- sqlite3_value_blob val
                         bytes <- sqlite3_value_bytes val
                         str <- packCStringLen (castPtr ptr, fromIntegral bytes)
                         return $ Blob str
       | otherwise -> fail "get_val: unknown type"

-- | This is the type of the function supported by the 'addRegexpSupport'
--   function. The first argument is the regular expression to match with
--   and the second argument is the string to match. The result shall be
--   'True' for successful match and 'False' otherwise.
type RegexpHandler = ByteString -> ByteString -> IO Bool

-- | This function registers a 'RegexpHandler' to be called when
--   REGEXP(regexp,str) is used in an SQL query.
addRegexpSupport :: SQLiteHandle -> RegexpHandler -> IO ()
addRegexpSupport h f =
 withCString "REGEXP" $ \ zFunctionName ->
  do xFunc <- mkStepHandler $ regexp_callback f
     withPrim h $ \ db ->
       sqlite3_create_function db zFunctionName 2 sQLITE_UTF8 nullPtr
                               xFunc noCallback noCallback
     addSQLiteHandleFinalizer h (freeCallback xFunc)

-- | Internal function to marshall the C types into Haskell types to
--   make a RegexpHandler compatible with the Sqlite3 API.
regexp_callback :: RegexpHandler -> StepHandler
regexp_callback f ctx argc argv =
  if argc /= 2 then return_fail else
  do arg0 <- sqlite3_value_cstringlen =<< peek argv
     arg1 <- sqlite3_value_cstringlen =<< peekElemOff argv 1
     if isNullCStringLen arg0 || isNullCStringLen arg1 then return_fail else
       do regexp_str <- unsafePackCStringLen arg0
          str        <- unsafePackCStringLen arg1
          res        <- f regexp_str str
          if res then return_success else return_fail
  where
  return_fail    = sqlite3_result_int ctx 0
  return_success = sqlite3_result_int ctx 1

isNullCStringLen :: CStringLen -> Bool
isNullCStringLen (p,_) = p == nullPtr

sqlite3_value_cstringlen :: SQLiteValue -> IO CStringLen
sqlite3_value_cstringlen v =
 do str <- sqlite3_value_text v
    len <- sqlite3_value_bytes v
    return (str, fromIntegral len)

----------
type Arity = Int
type FunctionName = String
type FunctionHandler = SQLiteContext -> [SQLiteValue] -> IO ()

class IsFunctionHandler a where 
    funcArity   :: a -> Arity
    funcHandler :: a -> FunctionHandler

instance IsValue r => IsFunctionHandler r where
    funcArity _         = 0
    funcHandler f ctx _ = returnSQLiteValue ctx f

instance IsValue r => IsFunctionHandler (String -> r) where
    funcArity _             = 1
    funcHandler f ctx (x:_) = returnSQLiteValue ctx =<< fmap f (fromSQLiteValue x)
    funcHandler _ ctx _     = returnSQLiteValue ctx ()

instance (IsValue a, IsValue r) => IsFunctionHandler (a -> r) where
    funcArity _             = 1
    funcHandler f ctx (x:_) = returnSQLiteValue ctx =<< fmap f (fromSQLiteValue x)
    funcHandler _ ctx _     = returnSQLiteValue ctx ()

instance (IsValue a, IsValue b, IsValue r) => IsFunctionHandler (a -> b -> r) where
    funcArity _                 = 2
    funcHandler f ctx (x:y:_)   = do
        x' <- fromSQLiteValue x
        y' <- fromSQLiteValue y
        returnSQLiteValue ctx $ f x' y'
    funcHandler _ ctx _         = returnSQLiteValue ctx ()

instance (IsValue a, IsValue b, IsValue c, IsValue r) => IsFunctionHandler (a -> b -> c -> r) where
    funcArity _                 = 3
    funcHandler f ctx (x:y:z:_) = do
        x' <- fromSQLiteValue x
        y' <- fromSQLiteValue y
        z' <- fromSQLiteValue z
        returnSQLiteValue ctx $ f x' y' z'
    funcHandler _ ctx _         = returnSQLiteValue ctx ()

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue r) => IsFunctionHandler (a -> b -> c -> d -> r) where
    funcArity _                   = 4
    funcHandler f ctx (x:y:z:w:_) = do
        x' <- fromSQLiteValue x
        y' <- fromSQLiteValue y
        z' <- fromSQLiteValue z
        w' <- fromSQLiteValue w
        returnSQLiteValue ctx $ f x' y' z' w'
    funcHandler _ ctx _         = returnSQLiteValue ctx ()

instance (IsValue a, IsValue r) => IsFunctionHandler ([a] -> r) where
    funcArity _            = -1
    funcHandler f ctx args = do
        args' <- mapM fromSQLiteValue args
        returnSQLiteValue ctx $ f args'

instance IsValue r => IsFunctionHandler (IO r) where
    funcArity _         = 0
    funcHandler f ctx _ = returnSQLiteValue ctx =<< f

instance IsValue r => IsFunctionHandler (String -> IO r) where
    funcArity _             = 1
    funcHandler f ctx (x:_) = returnSQLiteValue ctx =<< f =<< fromSQLiteValue x
    funcHandler _ ctx _     = returnSQLiteValue ctx ()

instance (IsValue a, IsValue r) => IsFunctionHandler (a -> IO r) where
    funcArity _             = 1
    funcHandler f ctx (x:_) = returnSQLiteValue ctx =<< f =<< fromSQLiteValue x
    funcHandler _ ctx _     = returnSQLiteValue ctx ()

instance (IsValue a, IsValue b, IsValue r) => IsFunctionHandler (a -> b -> IO r) where
    funcArity _                 = 2
    funcHandler f ctx (x:y:_)   = do
        x' <- fromSQLiteValue x
        y' <- fromSQLiteValue y
        returnSQLiteValue ctx =<< f x' y'
    funcHandler _ ctx _         = returnSQLiteValue ctx ()

instance (IsValue a, IsValue b, IsValue c, IsValue r) => IsFunctionHandler (a -> b -> c -> IO r) where
    funcArity _                 = 3
    funcHandler f ctx (x:y:z:_) = do
        x' <- fromSQLiteValue x
        y' <- fromSQLiteValue y
        z' <- fromSQLiteValue z
        returnSQLiteValue ctx =<< f x' y' z'
    funcHandler _ ctx _         = returnSQLiteValue ctx ()

instance (IsValue a, IsValue b, IsValue c, IsValue d, IsValue r) => IsFunctionHandler (a -> b -> c -> d -> IO r) where
    funcArity _                   = 4
    funcHandler f ctx (x:y:z:w:_) = do
        x' <- fromSQLiteValue x
        y' <- fromSQLiteValue y
        z' <- fromSQLiteValue z
        w' <- fromSQLiteValue w
        returnSQLiteValue ctx =<< f x' y' z' w'
    funcHandler _ ctx _         = returnSQLiteValue ctx ()

instance (IsValue a, IsValue r) => IsFunctionHandler ([a] -> IO r) where
    funcArity _            = -1
    funcHandler f ctx args = do
        args' <- mapM fromSQLiteValue args
        returnSQLiteValue ctx =<< f args'

createFunction :: IsFunctionHandler a => SQLiteHandle -> FunctionName -> a -> IO ()
createFunction h name f = createFunctionPrim h name (funcArity f) (funcHandler f)

function_callback :: FunctionHandler -> StepHandler
function_callback f ctx argc argv = do
    args <- peekArray (fromEnum argc) argv
    f ctx args

createFunctionPrim :: SQLiteHandle -> FunctionName -> Arity -> FunctionHandler -> IO ()
createFunctionPrim h name arity f = do
    xFunc <- mkStepHandler $ function_callback f
    withPrim h $ \db -> do
        withCString name $ \zFunctionName -> do
            sqlite3_create_function
                db
                zFunctionName
                (toEnum arity)
                sQLITE_UTF8
                nullPtr
                xFunc
                noCallback
                noCallback
    addSQLiteHandleFinalizer h (freeCallback xFunc)

finalize_callback :: IsValue v => a -> (a -> IO v) -> FinalizeContextHandler
finalize_callback x f ctx = do
    aVal <- get_aggr_context x ctx
    returnSQLiteValue ctx =<< f aVal

get_aggr_context :: a -> SQLiteContext -> IO a
get_aggr_context x ctx = do
    SQLiteContextBuffer aBuf <- sqlite3_aggregate_context ctx _SZ
    aPtr <- peek (castPtr aBuf)
    if aPtr == nullPtr then return x else do
        let sPtr = castPtrToStablePtr aPtr
        rv <- deRefStablePtr sPtr
        freeStablePtr sPtr
        return rv

set_aggr_context :: SQLiteContext -> a -> IO ()
set_aggr_context ctx x = do
    SQLiteContextBuffer aBuf <- sqlite3_aggregate_context ctx _SZ
    aPtr <- newStablePtr x
    poke (castPtr aBuf) $ castStablePtrToPtr aPtr

_SZ :: CInt
_SZ = toEnum $ sizeOf nullPtr
 
step_callback :: IsValue v => a -> (a -> [v] -> IO a) -> StepHandler
step_callback x f ctx argc argv = do
    args <- peekArray (fromEnum argc) argv
    aVal <- get_aggr_context x ctx
    newVal <- f aVal =<< mapM fromSQLiteValue args
    set_aggr_context ctx newVal 

createAggregatePrim :: (IsValue i, IsValue o) => SQLiteHandle -> FunctionName -> Arity -> (a -> [i] -> IO a) -> a -> (a -> IO o) -> IO ()
createAggregatePrim h name arity step x finalize = do
    stepFunc <- mkStepHandler $ step_callback x step
    finalizeFunc <- mkFinalizeContextHandler $ finalize_callback x finalize
    withPrim h $ \db -> do
        withCString name $ \zFunctionName -> do
            sqlite3_create_function
                db
                zFunctionName
                (toEnum arity)
                sQLITE_UTF8
                nullPtr
                noCallback
                stepFunc
                finalizeFunc
    addSQLiteHandleFinalizer h (freeCallback stepFunc)
    addSQLiteHandleFinalizer h (freeCallback finalizeFunc)
    
class IsValue a where
    fromSQLiteValue     :: SQLiteValue -> IO a
    returnSQLiteValue   :: SQLiteContext -> a -> IO ()

instance IsValue SQLiteValue where
    fromSQLiteValue     = return
    returnSQLiteValue   = sqlite3_result_value

instance IsValue Value where
    fromSQLiteValue     = sqlite3_value_value
    returnSQLiteValue ctx v = case v of
        Double d    -> returnSQLiteValue ctx d
        Int i       -> returnSQLiteValue ctx i
        Text s      -> returnSQLiteValue ctx s
        Blob b      -> returnSQLiteValue ctx b
        Null        -> returnSQLiteValue ctx ()

instance IsValue Double where
    fromSQLiteValue     = sqlite3_value_double
    returnSQLiteValue   = sqlite3_result_double

instance IsValue Int64 where
    fromSQLiteValue     = sqlite3_value_int64
    returnSQLiteValue   = sqlite3_result_int64

instance IsValue CInt where
    fromSQLiteValue     = sqlite3_value_int
    returnSQLiteValue   = sqlite3_result_int

instance IsValue Int where
    fromSQLiteValue     = fmap fromEnum . sqlite3_value_int
    returnSQLiteValue   = (. toEnum) . sqlite3_result_int

instance IsValue CStringLen where
    fromSQLiteValue                   = sqlite3_value_cstringlen
    returnSQLiteValue ctx (cptr, len) = sqlite3_result_text ctx cptr (toEnum len) sqlite3_static_destructor

instance IsValue String where
    fromSQLiteValue v = do
        cstrlen <- fromSQLiteValue v
        UTF8.decodeString `fmap` peekCStringLen cstrlen
    returnSQLiteValue ctx str = withCStringLen (UTF8.encodeString str) $ \(cptr, len) -> do
        sqlite3_result_text ctx cptr (toEnum len) sqlite3_transient_destructor

instance IsValue ByteString where
    fromSQLiteValue = unsafePackCStringLen <=< fromSQLiteValue
    returnSQLiteValue ctx bytes = unsafeUseAsCStringLen bytes $ \(cptr, len) -> do
        sqlite3_result_text ctx cptr (toEnum len) sqlite3_transient_destructor

instance IsValue () where
    fromSQLiteValue _       = return ()
    returnSQLiteValue ctx _ = sqlite3_result_null ctx

instance IsValue a => IsValue (Maybe a) where
    fromSQLiteValue v       = do
        typ <- sqlite3_value_type v
        if typ == sQLITE_NULL
            then return Nothing
            else fmap Just (fromSQLiteValue v)
    returnSQLiteValue ctx v = case v of
        Just v' -> returnSQLiteValue ctx v'
        _       -> returnSQLiteValue ctx ()

