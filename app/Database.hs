{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database (TodoItemRow(..), migrateAll, toRow, fromRow, getAll) where

import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Text (Text)
import Model (TodoItem(..))
share 
  [mkPersist sqlSettings, mkMigrate "migrateAll"] 
  [persistLowerCase|
TodoItemRow
  title       Text
  description Text Maybe
  completed   Bool
  deriving Show
|]


toRow :: TodoItem -> TodoItemRow
toRow TodoItem {..} = TodoItemRow {todoItemRowTitle = todoItemTitle, todoItemRowDescription = todoItemDescription, todoItemRowCompleted = todoItemCompleted}

fromRow :: Entity TodoItemRow -> TodoItem
fromRow Entity {entityKey, entityVal = TodoItemRow {..}} =
  TodoItem
    { todoItemIdentifier = Just $ fromSqlKey entityKey
    , todoItemTitle = todoItemRowTitle
    , todoItemDescription = todoItemRowDescription
    , todoItemCompleted = todoItemRowCompleted
    }
    
getAll :: SqlPersistT IO [TodoItem]
getAll = do
  (todoItemRows :: [Entity TodoItemRow]) <- selectList [] [LimitTo 10]
  return $ fromRow <$> todoItemRows