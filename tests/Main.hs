module Main where

import Database.SQLite

newTable :: TableName -> Table SQLType
newTable tName = 
  Table { tabName    = tName
        , tabColumns = 
            [ Column { colName    = "id"
                     , colType    = SQLInt NORMAL False False
                     , colClauses = [PrimaryKey,AutoIncrement]
                     }
            , Column { colName    = "name"
                     , colType    = SQLVarChar 200
                     , colClauses = [IsNullable False]
                     }
            ]
        , tabConstraints = []
        }

main :: IO ()
main = do
  h <- openConnection "test.db"
  defineTable h (newTable "names")
  insertRow h "Poststed" [("Postnr", "4276"),("Poststed", "Vea")]
  ls <- execStatement h "SELECT * FROM Poststed;"
  print ls
  closeConnection h

