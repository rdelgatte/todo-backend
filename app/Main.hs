{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Logger
import Data.Int (Int64)
import Database (EntityField (TodoItemRowIsCompleted), TodoItemRow (..), fromRow, migrateAll, mkTodoItemRowId, toRow)
import Database.Persist (getEntity)
import qualified Database.Persist as Sql
import Database.Persist.Sqlite (Entity, PersistStoreWrite (insert_), runMigration, runSqlConn, selectList, withSqliteConn)
import Model (TodoItem (..), TodoItemSpec (..))
import Network.HTTP.Types.Method (methodDelete, methodPatch)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (cors, corsMethods, corsRequestHeaders, simpleCorsResourcePolicy, simpleHeaders)
import Web.Scotty

corsMiddleware :: Middleware
corsMiddleware =
  cors . const . Just $
    simpleCorsResourcePolicy
      { corsMethods =
          methodPatch
            : methodDelete
            : corsMethods simpleCorsResourcePolicy,
        corsRequestHeaders = simpleHeaders
      }

main :: IO ()
main = runNoLoggingT $
  withSqliteConn ":memory:" $ \sqlBackend -> NoLoggingT $ do
    runSqlConn (runMigration migrateAll) sqlBackend
    scotty 3000 $ do
      middleware corsMiddleware
      get "/items" $ do
        maybeCompleted <- queryParamMaybe @Bool "completed"
        case maybeCompleted of
          Just completed -> do
            (todoItemRows :: [Entity TodoItemRow]) <-
              runSqlConn
                ( selectList
                    [ TodoItemRowIsCompleted Sql.==. completed
                    ]
                    []
                )
                sqlBackend
            json $ fromRow <$> todoItemRows
          Nothing -> do
            (todoItemRows :: [Entity TodoItemRow]) <- runSqlConn (selectList [] []) sqlBackend
            json $ fromRow <$> todoItemRows

      get "/items/:id" $ do
        identifier <- pathParam @Int64 "id"
        (maybeTodoItemRow :: Maybe (Entity TodoItemRow)) <- runSqlConn (getEntity $ mkTodoItemRowId identifier) sqlBackend
        case maybeTodoItemRow of
          Nothing -> html "nothing"
          Just todoItemRow -> json $ fromRow todoItemRow

      post "/items" $ do
        (spec :: TodoItemSpec) <- jsonData @TodoItemSpec
        let newItem =
              TodoItem
                { todoItemIdentifier = Nothing,
                  todoItemTitle = todoItemSpecTitle spec,
                  todoItemDescription = todoItemSpecDescription spec,
                  todoItemCompleted = False
                }
        runSqlConn (insert_ $ toRow newItem) sqlBackend

      patch "/items/:id/complete" $ do
        identifier <- pathParam @Int64 "id"
        runSqlConn
          ( Sql.update
              (mkTodoItemRowId identifier)
              [ TodoItemRowIsCompleted Sql.=. True
              ]
          )
          sqlBackend
