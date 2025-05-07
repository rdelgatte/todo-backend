{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database where

import Data.Int (Int64)
import Data.Text (Text)
import Database.Persist.Sqlite
import Database.Persist.TH
import Model (TodoItem (..))


share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
TodoItemRow
  title       Text
  description Text Maybe
  isCompleted   Bool sql=is_deleted
|]

mkTodoItemRowId :: Int64 -> TodoItemRowId
mkTodoItemRowId = TodoItemRowKey . fromIntegral

toRow :: TodoItem -> TodoItemRow
toRow TodoItem {..} = TodoItemRow {todoItemRowTitle = todoItemTitle, todoItemRowDescription = todoItemDescription, todoItemRowIsCompleted = todoItemCompleted}

fromRow :: Entity TodoItemRow -> TodoItem
fromRow Entity {entityKey, entityVal = TodoItemRow {..}} =
  TodoItem
    { todoItemIdentifier = Just $ fromSqlKey entityKey,
      todoItemTitle = todoItemRowTitle,
      todoItemDescription = todoItemRowDescription,
      todoItemCompleted = todoItemRowIsCompleted
    }

getAll :: SqlPersistT IO [TodoItem]
getAll = do
  (todoItemRows :: [Entity TodoItemRow]) <- selectList [] [LimitTo 10]
  return $ fromRow <$> todoItemRows