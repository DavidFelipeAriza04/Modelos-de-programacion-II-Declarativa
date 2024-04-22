{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int String deriving (Show)

instance FromRow TestField where fromRow = TestField <$> field <*> field

main :: IO ()
main = do
  conn <- open "Empresa.db"
  r <- query_ conn "SELECT employee_id, first_name from employees" :: IO [TestField]
  mapM_ print r
  close conn