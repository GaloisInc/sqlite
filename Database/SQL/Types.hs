--------------------------------------------------------------------
-- |
-- Module    : Database.SQL.Types
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
-- Basic embedding of SQL types in Haskell.
--
-- Note: the quary part of this modules was imported (with modifications)
-- from the lowest layer of abstraction of HaskellDB.
module Database.SQL.Types
       ( TableName
       , ColumnName
       , DatabaseName
       , OpName

       , SQLOrder(..)
       , SQLSelect(..)
       , select_all
       , SelectSource(..)
       , Join(..)
       , TableSource(..)
       , SQLExpr(..)
       , SQLUpdate(..)
       , SQLDelete(..)
       , SQLInsert(..)
       , SQLCreate(..)
       , SQLDrop(..)

       , Clause(..)
       , Constraint(..)
       , Table(..)
       , Column(..)
       , SQLTable

       , SQLType(..)
       , IntType(..)
       , DateTimeType(..)
       , BlobType(..)

       , showType
       , showClause
       , toSQLString
       , export_sql
       , PrettySQL(..)
       ) where

import Data.List ( intersperse )
import Text.PrettyPrint.HughesPJ

type DatabaseName = String
type TableName = String
type ColumnName = String
type OpName     = String

data Clause
 = IsNullable Bool
 | DefaultValue String
 | PrimaryKey Bool    -- ^ Auto-increment?
 | ForeignKey TableName [ColumnName]
 | Clustered Bool
 | Unique

data Constraint
  = TablePrimaryKey [ColumnName]
  | TableUnique [ColumnName]
  | TableCheck SQLExpr

data Table a
 = Table { tabName        :: String
         , tabColumns     :: [Column a]
         , tabConstraints :: [Constraint]
         }
 | VirtualTable
         { tabName        :: String
         , tabColumns     :: [Column a]
         , tabConstraints :: [Constraint]
         , tabUsing       :: String
         }

type SQLTable = Table SQLType

-- | We parameterize over column type, since SQL engines
-- do tend to provide their own set of supported datatypes
-- (which may or may not map onto SQL99's set of types.)
data Column a
 = Column { colName    :: ColumnName
          , colType    :: a
          , colClauses :: [Clause]
          }

-- | MySQL slanted, but also SQLite friendly if you don't get
-- too fancy..
data SQLType
 = SQLBoolean
 | SQLChar    (Maybe Int)
 | SQLVarChar Int
 | SQLBlob     BlobType
 | SQLDateTime DateTimeType
 | SQLInt      IntType Bool{-unsigned?-} Bool{-zero fill-}
 | SQLDecimal  (Maybe Int){-total number of digits-}
               (Maybe Int){-digits after dec. point (the scale)-}
 | SQLFloat    (Maybe Int){-total number of digits-}
               (Maybe Int){-digits following dec. point-}
 | SQLEnum     [String]
 | SQLSet      [String]

data IntType
 = TINY | SMALL | MEDIUM | NORMAL | BIG

data DateTimeType
 = DATE | DATETIME | TIMESTAMP | TIME | YEAR (Maybe Int)

data BlobType
 = TinyBlob
 | NormalBlob (Maybe Int)
 | MediumBlob
 | LongBlob

showType :: SQLType -> String
showType t =
  case t of
    SQLBoolean          -> "BOOLEAN"
    SQLChar    Nothing  -> "CHAR"
    SQLChar    (Just x) -> "CHAR("++shows x ")"
    SQLVarChar x        -> "VARCHAR("++shows x ")"
    SQLBlob    bt       ->
      case bt of
         TinyBlob            -> "TINYBLOB"
         NormalBlob Nothing  -> "BLOB"
         NormalBlob (Just x) -> "BLOB("++shows x ")"
         MediumBlob          -> "MEDIUMBLOB"
         LongBlob            -> "LONGBLOB"

    SQLDateTime dt ->
      case dt of
         DATE -> "DATE"
         DATETIME  -> "DATETIME"
         TIMESTAMP -> "TIMESTAMP"
         TIME      -> "TIME"
         YEAR Nothing -> "YEAR"
         YEAR (Just x) -> "YEAR(" ++ shows x ")"
    SQLInt it unsigned zeroFill ->
      (if unsigned then (++" UNSIGNED") else id) $
       (if zeroFill then (++" ZEROFILL") else id) $
        (case it of
          TINY   -> "TINYINT"
          SMALL  -> "SMALLINT"
          MEDIUM -> "MEDIUMINT"
          NORMAL -> "INTEGER"
          BIG    -> "BIGINT")
    SQLDecimal mbDig mbScale -> 
        "DECIMAL" ++ 
        case sequence [mbDig,mbScale] of 
           Nothing -> ""
           Just xs -> '(':concat (intersperse "," (map show xs)) ++ ")"
    SQLFloat mbDig mbScale -> 
        "FLOAT" ++ 
        case sequence [mbDig,mbScale] of 
           Nothing -> ""
           Just xs -> '(':concat (intersperse "," (map show xs)) ++ ")"
    SQLEnum tgs ->  
        "ENUM(" ++ toTags tgs ++ ")"
    SQLSet tgs -> 
        "SET(" ++ toTags tgs ++ ")"
  where
    toTags xs = concat $ intersperse "," (map quote xs)

    quote nm = '\'':nm ++ "'"

showClause :: Clause -> String
showClause c = 
  case c of 
    IsNullable flg 
      | flg       -> "NULL"
      | otherwise -> "NOT NULL"
    DefaultValue x -> "DEFAULT " ++ toSQLString x
    PrimaryKey auto -> "PRIMARY KEY" ++ if auto then " AUTOINCREMENT" else ""
    ForeignKey tb cs -> "FOREIGN KEY " ++ tb ++ '(':concat (intersperse ", " cs) ++ ")"
    Clustered flg
      | flg -> "CLUSTERED"
      | otherwise -> "NONCLUSTERED"
    Unique  -> "UNIQUE"

toSQLString :: String -> String
toSQLString "" = ""
toSQLString ('\'':xs) = '\'':'\'':toSQLString xs
toSQLString (x:xs) = x : toSQLString xs

--------------------------------------------------------------------------------




data SQLOrder = SQLAsc | SQLDesc

-- | Data type for SQL SELECT statements.
data SQLSelect  = SQLSelect
    { options   :: [String]                 -- ^ DISTINCT, ALL etc.
    -- | result, alias.  Empty list means "select all".
    , attrs     :: [(SQLExpr,String)]
    , tables    :: SelectSource             -- ^ FROM
    , criteria  :: [SQLExpr]                -- ^ WHERE
    , groupby   :: [SQLExpr]                -- ^ GROUP BY
    , orderby   :: [(SQLExpr,SQLOrder)]     -- ^ ORDER BY
    , extra     :: [String]                 -- ^ TOP n, etc.
    }
  | SQLBin OpName SQLSelect SQLSelect       -- ^ UNION, etc

select_all :: SelectSource -> SQLSelect
select_all src = SQLSelect { options = ["DISTINCT"]
                           , attrs = []
                           , tables = src
                           , criteria = []
                           , groupby = []
                           , orderby = []
                           , extra = []
                           }

data SelectSource = From TableSource [Join]

-- | Join with another table.
data Join         = Join OpName TableSource (Maybe (OpName ,SQLExpr))

-- | Use empty string for no alias.
data TableSource  = SrcTable TableName String
                  | SrcSelect SQLSelect String

-- | Expressions in SQL statements.
data SQLExpr      = ColumnSQLExpr  ColumnName
                  | BinSQLExpr     OpName SQLExpr SQLExpr
                  | PrefixSQLExpr  OpName SQLExpr
                  | PostfixSQLExpr OpName SQLExpr
                  | FunSQLExpr     OpName [SQLExpr]
                  | ConstSQLExpr   String
                  | CaseSQLExpr    [(SQLExpr,SQLExpr)] SQLExpr
                  | ListSQLExpr    [SQLExpr]

-- | Data type for SQL UPDATE statements.
data SQLUpdate    = SQLUpdate TableName [(ColumnName,SQLExpr)] [SQLExpr]

-- | Data type for SQL DELETE statements.
data SQLDelete    = SQLDelete TableName [SQLExpr]

-- | Data type for SQL INSERT statements.
data SQLInsert    = SQLInsert      TableName [ColumnName] [SQLExpr]
                  | SQLInsertQuery TableName [ColumnName] SQLSelect

-- | Data type for SQL CREATE statements.
data SQLCreate a  = SQLCreateDB DatabaseName -- ^ Create a database
                  | SQLCreateTable (Table a) -- ^ Create a table

-- | Data type representing the SQL DROP statement.
data SQLDrop      = SQLDropDB DatabaseName -- ^ Delete a database
                  | SQLDropTable TableName -- ^ Delete a table named SQLTable



--------------------------------------------------------------------------------




class PrettySQL t where
  pp_sql :: t -> Doc

export_sql :: (PrettySQL t) => t -> String
export_sql x = render (pp_sql x)

instance PrettySQL SQLSelect     where pp_sql = ppSelect
instance PrettySQL SQLUpdate     where pp_sql = ppUpdate
instance PrettySQL SQLDelete     where pp_sql = ppDelete
instance PrettySQL SQLInsert     where pp_sql = ppInsert
instance PrettySQL a => PrettySQL (SQLCreate a) where pp_sql = ppCreate pp_sql
instance PrettySQL SQLDrop       where pp_sql = ppDrop

instance PrettySQL SQLType where pp_sql = text . showType


-- * SELECT

-- | Pretty prints a 'SQLSelect'
ppSelect :: SQLSelect -> Doc
ppSelect (SQLSelect opts as src crit group order other)
    = text "SELECT"
      <+> hsep (map text opts)
      <+> ppAttrs as
      $$ ppSelectSource src
      $$ ppWhere crit
      $$ ppGroupBy group
      $$ ppOrderBy order
      $$ hsep (map text other)
ppSelect (SQLBin op q1 q2) = parens (ppSelect q1) $$ text op $$ parens (ppSelect q2)

ppAttrs :: [(SQLExpr,ColumnName)] -> Doc
ppAttrs [] = text "*"
ppAttrs xs = commaV nameAs xs
    where
      -- | Print a name-value binding, or just the name if
      --   name and value are the same.
      nameAs :: (SQLExpr,ColumnName) -> Doc
      nameAs (ColumnSQLExpr c, name) | name == c = text name
      nameAs (expr, name) = ppSQLExpr expr <+> ppAlias name


ppSelectSource :: SelectSource -> Doc
ppSelectSource (From t js) = text "FROM" <+> ppTableSource t
                                         <+> vcat (map ppJoin js)

ppJoin :: Join -> Doc
ppJoin (Join op s a) = text op <+> ppTableSource s <+> ppJoinArg a

ppJoinArg :: Maybe (String,SQLExpr) -> Doc
ppJoinArg Nothing       = empty
ppJoinArg (Just (op,e)) = text op <+> ppSQLExpr e

ppTableSource :: TableSource -> Doc
ppTableSource (SrcTable x a)  = text x <+> ppAlias a
ppTableSource (SrcSelect s a) = parens (ppSelect s) <+> ppAlias a

ppAlias :: String -> Doc
ppAlias ""  = empty
ppAlias as  = text "AS" <+> text as

ppWhere :: [SQLExpr] -> Doc
ppWhere [] = empty
ppWhere es = text "WHERE"
             <+> hsep (intersperse (text "AND") (map ppSQLExpr es))

ppGroupBy :: [SQLExpr] -> Doc
ppGroupBy [] = empty
ppGroupBy es = text "GROUP BY" <+> commaV ppSQLExpr es

ppOrderBy :: [(SQLExpr,SQLOrder)] -> Doc
ppOrderBy [] = empty
ppOrderBy ord = text "ORDER BY" <+> commaV ppOrd ord
    where
      ppOrd (e,o) = ppSQLExpr e <+> ppSQLOrder o
      ppSQLOrder :: SQLOrder -> Doc
      ppSQLOrder SQLAsc = text "ASC"
      ppSQLOrder SQLDesc = text "DESC"


-- * UPDATE

-- | Pretty prints a 'SQLUpdate'
ppUpdate :: SQLUpdate -> Doc
ppUpdate (SQLUpdate name assigns crit)
        = text "UPDATE" <+> text name
        $$ text "SET" <+> commaV ppAssign assigns
        $$ ppWhere crit
    where
      ppAssign (c,e) = text c <+> equals <+> ppSQLExpr e


-- * DELETE

-- | Pretty prints a 'SQLDelete'
ppDelete :: SQLDelete -> Doc
ppDelete (SQLDelete name crit) =
    text "DELETE FROM" <+> text name $$ ppWhere crit


-- * INSERT

ppInsert :: SQLInsert -> Doc

ppInsert (SQLInsert table names values)
    = text "INSERT INTO" <+> text table
      <+> parens (commaV text names)
      $$ text "VALUES" <+> parens (commaV ppSQLExpr values)

ppInsert (SQLInsertQuery table names select)
    = text "INSERT INTO" <+> text table
      <+> parens (commaV text names)
      $$ ppSelect select


-- * CREATE

-- | Pretty prints a 'SQLCreate'.
ppCreate :: (a -> Doc) -> SQLCreate a -> Doc
ppCreate _ (SQLCreateDB name) = text "CREATE DATABASE" <+> text name
ppCreate ppType (SQLCreateTable t)
  = createTable (text (tabName t))
      <+> parens (vcat $ punctuate comma
                       $ map (ppColumn ppType) (tabColumns t) ++
                         map ppConstraint (tabConstraints t)
                 )
   where
   createTable n = case t of
        Table{} -> text "CREATE TABLE" <+> n
        VirtualTable{} -> hsep
            [ text "CREATE VIRTUAL TABLE"
            , n
            , text "USING"
            , text (tabUsing t)
            ]

ppColumn :: (a -> Doc) -> Column a -> Doc
ppColumn ppType c = text (colName c)
                <+> ppType (colType c)
                <+> hsep (map ppClause (colClauses c))

ppClause :: Clause -> Doc
ppClause c = text (showClause c)

ppConstraint :: Constraint -> Doc
ppConstraint c = case c of
  TablePrimaryKey cs -> text "PRIMARY KEY" <+> parens (commaH text cs)
  TableUnique cs     -> text "UNIQUE" <+> parens (commaH text cs)
  TableCheck e       -> text "CHECK" <+> (ppSQLExpr e)




-- * DROP

-- | Pretty prints a 'SQLDrop'.
ppDrop :: SQLDrop -> Doc
ppDrop (SQLDropDB name) = text "DROP DATABASE" <+> text name
ppDrop (SQLDropTable name) = text "DROP TABLE" <+> text name


-- * Expressions

-- | Pretty prints a 'SQLExpr'
ppSQLExpr :: SQLExpr -> Doc
ppSQLExpr e =
    case e of
      ColumnSQLExpr c     -> text c
      BinSQLExpr op e1 e2 -> ppSQLExpr e1 <+> text op <+> ppSQLExpr e2
      PrefixSQLExpr op e1 -> text op <+> ppSQLExpr e1
      PostfixSQLExpr op e1-> ppSQLExpr e1 <+> text op
      FunSQLExpr f es     -> text f <> parens (commaH ppSQLExpr es)
      ConstSQLExpr c      -> text c
      CaseSQLExpr cs el   -> text "CASE" <+> vcat (map ppWhen cs)
                             <+> text "ELSE" <+> ppSQLExpr el <+> text "END"
          where ppWhen (w,t) = text "WHEN" <+> ppSQLExpr w
                               <+> text "THEN" <+> ppSQLExpr t
      ListSQLExpr es      -> parens (commaH ppSQLExpr es)

commaH :: (a -> Doc) -> [a] -> Doc
commaH f = hcat . punctuate comma . map f

commaV :: (a -> Doc) -> [a] -> Doc
commaV f = vcat . punctuate comma . map f
