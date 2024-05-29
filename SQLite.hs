{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Aeson (ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy.IO as I
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import GHC.Generics

data Person = Person {p_id :: Int, name :: String, last_name :: String, salary :: Integer} deriving (Show, Generic, ToJSON)

data Department = Department {d_id :: Int, dept_name :: String, loc_id :: Int} deriving (Show, Generic, ToJSON)

data PersonRow = PersonRow Int String String Integer deriving (Show)

data DeptRow = DeptRow Int String Int deriving (Show)

instance FromRow PersonRow where fromRow = PersonRow <$> field <*> field <*> field <*> field

instance FromRow DeptRow where fromRow = DeptRow <$> field <*> field <*> field

createPersonList :: [PersonRow] -> [Person]
createPersonList [] = []
createPersonList ((PersonRow i n l s) : t) =
  Person {p_id = i, name = n, last_name = l, salary = s} : createPersonList t

createDepartmentList :: [DeptRow] -> [Department]
createDepartmentList [] = []
createDepartmentList ((DeptRow i n l) : t) =
  Department {d_id = i, dept_name = n, loc_id = l} : createDepartmentList t

main :: IO ()
main = do
  conn <- open "ProyectoFinal/DB.db"
  p <- query_ conn "SELECT employee_id, first_name, last_name, salary from employees order by salary desc LIMIT 4" :: IO [PersonRow]
  let persons = createPersonList p
  mapM_ print persons
  d <- query_ conn "SELECT department_id, department_name, location_id from departments" :: IO [DeptRow]
  let departments = createDepartmentList d
  mapM_ print departments
  I.writeFile "ProyectoFinal/Data.json" (encodeToLazyText persons)
  I.writeFile "ProyectoFinal/Departments.json" (encodeToLazyText departments)
  close conn