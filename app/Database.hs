{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Scientific
import Data.Text
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Todo.Types

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase |
  ItemRow
    title Text
    deriving Show
|]

type DatabaseIO = ReaderT SqlBackend IO

saveItem :: Item -> DatabaseIO ()
saveItem Item = do
  ItemId <- insert $ toRow Item
  liftIO . putStrLn $ "Just saved Item " <> show Item <> " and its database ID is " <> show ItemId

getAllItems :: DatabaseIO [Item]
getAllItems = do
  Items :: [Entity ItemRow] <- selectList [] []
  liftIO . putStrLn $ "Items in database are: " <> show Items
  return $ fromRow <$> Items

toRow :: Item -> ItemRow
toRow Item {..} = ItemRow {ItemRowTitle = title, ItemRowDescription = description, ItemRowCompleted = completed}

fromRow :: Entity ItemRow -> Item
fromRow Entity {entityKey, entityVal = ItemRow {..}} =
  Item
    { identifier = Just $ fromSqlKey entityKey
    , title = ItemRowTitle
    , description = ItemRowDescription
    , completed = ItemRowCompleted
    }