{-# LANGUAGE OverloadedStrings          #-}

import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO, MonadIO (liftIO))
import Database (TodoItemRow(..), migrateAll, toRow)
import Model (TodoItem(..))
main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll
    
    itemId <- insert $ toRow $ TodoItem Nothing "title" (Just "anything") False

    -- read from db
    item <- get itemId

    liftIO $ print (item :: Maybe TodoItemRow)
    
    -- delete from db
    delete itemId 