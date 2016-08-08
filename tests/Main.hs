module Main where

import Control.Exception.Base (bracket)
import Data.Maybe
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Hspec
import Text.Printf (printf)

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

withDB :: FilePath -> (SQLiteHandle -> IO a) -> IO a
withDB dbName = bracket (openConnection dbName) closeConnection

withTempDB :: (SQLiteHandle -> IO a) -> IO a
withTempDB f =
    withTempDirectory "tmp" "test." $ \dirname ->
    let dbName = dirname </> "database.sqlite3" in
    withDB dbName f


withTempTable :: (String -> SQLiteHandle -> IO a) -> IO a
withTempTable f = withTempDB $ \h ->
    let tab = "names" in
    defineTable h (newTable "names") >> f tab h


flatExecStatement :: SQLiteResult a => SQLiteHandle -> String -> IO (Either String [Row a])
flatExecStatement h sqlStmt =
    let flattenResults = concat
        mapResult f (Right r) = Right (f r)
        mapResult f (Left l) = Left l

    in mapResult flattenResults <$> execStatement h sqlStmt


insertManyRows :: SQLiteHandle -> String -> [Row String] -> IO (Maybe String)
insertManyRows h tab rows = chain insertions
    where insertions   = map (insertRow h tab) rows

          chain []     = return Nothing
          chain (i:is) = do
            r <- i
            case r of
                Nothing -> chain is
                Just err -> return $ Just err


spec :: Spec
spec = parallel $ do
    describe "execStatement and execStatement_" $ do
        it "runs select statements" $ withTempDB $ \h -> do
            result <- flatExecStatement h "SELECT 'Hello, World' AS h"
            result `shouldBe` Right [[("h", "Hello, World")]]

        it "fails on bad SQL" $ withTempDB $ \h -> do
            error <- execStatement_ h "SELECT aieauie"
            error `shouldSatisfy` isJust


    describe "insertRow" $ do
        it "stores data" $ withTempTable $ \tab h -> do
            let row = [("id", "1"), ("name", "John Doe"), ("age", "45")]
            error <- insertRow h tab row
            error `shouldSatisfy` isNothing

            ls <- flatExecStatement h $ printf "SELECT * FROM %s" tab
            ls `shouldBe` Right [row]


        it "can be called many times" $ withTempTable $ \tab h -> do
            let rows = [ [ ("id", "1"), ("name", "Erika Munstermann"), ("age", "28") ]
                       , [ ("id", "2"), ("name", "Max Munstermann"), ("age", "24") ]
                       ]
            error <- insertManyRows h tab rows
            error `shouldSatisfy` isNothing

            ls <- flatExecStatement h $ printf "SELECT * FROM %s ORDER BY id" tab
            ls `shouldBe` Right rows


        it "fails on bad row insertion" $ withTempTable $ \tab h -> do
            error <- insertRow h tab [("foo", "bar")]
            error `shouldSatisfy` isJust

    describe "createFunction" $ do
        it "runs haskell code" $ withTempDB $ \h -> do
            createFunction h "hi" (sum :: [Int] -> Int)
            ls <- flatExecStatement h "SELECT hi(1, 2, 3, 4, 5) AS greeting"
            ls `shouldBe` Right [[("greeting", show $ sum [1, 2, 3, 4, 5])]]


main :: IO ()
main = hspec spec
