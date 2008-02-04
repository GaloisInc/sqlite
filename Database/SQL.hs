module Database.SQL where

import Database.SQL.Types

sqlInsert :: TableName -> [(ColumnName,SQLExpr)] -> SQLInsert
sqlInsert t xs  = let (as,bs) = unzip xs
                  in SQLInsert t as bs

