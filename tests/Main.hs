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
	    , Column { colName    = "age"
	             , colType    = SQLVarChar 200
		     , colClauses = [IsNullable True]
		     }
            ]
        , tabConstraints = []
	}

main :: IO ()
main = do
  let dbFile = "test.db"
  let ign act = 
        catch (act >> return ()) 
              (\ err -> putStrLn ("Error (but ignoring) -- " ++ show err) >> return ())
  h <- openConnection dbFile
  ign $ defineTable h (newTable "names")
  ign $ insertRow h "Poststed" [("Postnr", "4278"),("Poststed", "Veakrossen")]
  ls <- execStatement h "SELECT * FROM Poststed WHERE PostNr=4276;"
  print ls
  ign $ insertRow h "names" [("id", "1"),("name", "foo"), ("age", "30")]
  ls <- execStatement h "SELECT * FROM names;"
  print ls
  ls <- execStatement h "SELECT * FROM names where id=1;"
  print ls
  closeConnection h

