--------------------------------------------------------------------
-- |
-- Module    : DB.SQL.Types
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
-- Basic embedding of SQL types in Haskell.
--
module DB.SQL.Types
       ( TableName
       , ColumnName

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

       ) where

import Data.List ( intersperse )

type TableName = String
type ColumnName = String

data Clause
 = IsNullable Bool
 | DefaultValue String
 | AutoIncrement
 | PrimaryKey
 | ForeignKey TableName [ColumnName]
 | Clustered Bool
 | Unique

data Constraint
 = Constraint String [ColumnName] Clause

data Table a
 = Table { tabName        :: String
         , tabColumns     :: [Column a]
         , tabConstraints :: [Constraint]
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
    AutoIncrement  -> "AUTOINCREMENT"
    PrimaryKey     -> "PRIMARY KEY"
    ForeignKey tb cs -> "FOREIGN KEY " ++ tb ++ '(':concat (intersperse ", " cs) ++ ")"
    Clustered flg
      | flg -> "CLUSTERED"
      | otherwise -> "NONCLUSTERED"
    Unique  -> "UNIQUE"

toSQLString :: String -> String
toSQLString "" = ""
toSQLString ('\'':xs) = '\'':'\'':toSQLString xs
toSQLString (x:xs) = x : toSQLString xs

