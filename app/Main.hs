{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger
import Control.Monad.Reader
import Database
import Database.Persist.Sqlite (runMigration, withSqliteConn)
import Web.Scotty

main :: IO ()
main = runNoLoggingT $
  withSqliteConn ":memory:" $ \sqlBackend -> do
    runReaderT (runMigration migrateAll) sqlBackend -- Do the database migration once
    NoLoggingT $
      scotty 3000 $
        get "/:word" $ do
          beam <- pathParam "word"
          html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
