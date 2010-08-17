module Main where

import Database.SQLite

newTable :: TableName -> Table SQLType
newTable tName = 
  VirtualTable
        { tabName    = tName
        , tabColumns = 
	    [ Column { colName    = "id"
	             , colType    = SQLInt NORMAL False False
		     , colClauses = [PrimaryKey True]
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
        , tabUsing = "FTS3"
	}

main :: IO ()
main = do
  let dbFile = "test.db"
  let ign act = 
        catch (act >> return ()) 
              (\ err -> putStrLn ("Error (but ignoring) -- " ++ show err) >> return ())
  h <- openConnection dbFile
  ign $ defineTable h (newTable "names")
  --
  ign $ insertRow h "Poststed" [("Postnr", "4278"),("Poststed", "Veakrossen")]
  ls <- execStatement h "SELECT * FROM Poststed WHERE PostNr=4276;"
  print (ls :: Either String [[Row Value]])
  --
  ign $ insertRow h "names" [("id", "2357"), ("name", "1,5m bis 3,0m"), ("age","35")]
  -- This test a bug reported by Nikolas Mayr
  ls <- execStatement h "SELECT * FROM names WHERE id=2357;"
  print (ls :: Either String [[Row Value]])
  --
  ign $ insertRow h "names" [("id", "1"),("name", "foo"), ("age", "30")]
  ls <- execStatement h "SELECT * FROM names;"
  print (ls :: Either String [[Row String]])
  --
  ls <- execStatement h "SELECT * FROM names where id=1;"
  print (ls :: Either String [[Row String]])
  --
  createFunction h "hi" (sum :: [Int] -> Int)
  ls <- execStatement h "SELECT hi(1,2,3,4,5) AS greeting"
  print (ls :: Either String [[Row String]])
  --
  createFunction h "say" putStrLn
  ls <- execStatement h "SELECT say('Hi') AS greeting"
  print (ls :: Either String [[Row String]])
  --
  closeConnection h

