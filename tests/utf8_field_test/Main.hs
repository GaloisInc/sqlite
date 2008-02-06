import System.Directory
import System.IO.Error
import System.Exit
import Database.SQLite

check m = do x <- m
             case x of
               Left err -> ioError (userError err)
               Right a  -> return a

-- Makes a blank database.
withDB m = do let db_file = "test.db"
              db <- openConnection db_file
              a <- m db
              closeConnection db
              removeFile db_file
              return a


vals = [ "normal"
       , "the symbol for forall is \x2200"
       , "zero \0 in the middle"
       ]

insert db x = check $ execParamStatement db
                        "INSERT INTO T (x) VALUES (:val)" [(":val", Text x)]

main = withDB $ \db ->
          do check $ execStatement db "CREATE TABLE T (x text)"
             mapM (insert db) vals
             rs <- check $ execStatement db "SELECT * from T"
             let new_vals = map (snd . head) rs
             if vals == new_vals
                then putStrLn "UTF8 test passed."
                else do putStrLn "UTF8 test failed."
                        putStrLn "*** Expected:"
                        mapM_ print vals
                        putStrLn "*** Result:"
                        mapM_ print new_vals

