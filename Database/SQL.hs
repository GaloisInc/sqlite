module Database.SQL (module Database.SQL, module Database.SQL.Types) where

import Database.SQL.Types

sqlInsert :: TableName -> [(ColumnName,SQLExpr)] -> SQLInsert
sqlInsert t xs  = let (as,bs) = unzip xs
                  in SQLInsert t as bs

