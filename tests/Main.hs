module Main where

import SQLite

main :: IO ()
main = do
  h <- openConnection "test.db"
  insertRow h "Poststed" [("Postnr", "4276"),("Poststed", "Vea")]
  ls <- execStatement h "SELECT * FROM Poststed;"
  print ls
  closeConnection h

