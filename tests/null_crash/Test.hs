module Main where

import SQLite

main :: IO ()
main  = do
  h  <- openConnection "test.db"
  rs <- execStatement h "select * from feed_attrs where feed_attr_fid = 1;"
  print rs
  closeConnection h
